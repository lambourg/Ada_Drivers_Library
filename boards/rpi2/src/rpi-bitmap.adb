------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;
with Interfaces;               use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;              use Ada.Text_IO;

with RPi.Regs.DMA;             use RPi.Regs.DMA;
with RPi.Firmware.GPU_Memory;  use RPi.Firmware.GPU_Memory;

package body RPi.Bitmap is

   Debug : constant Boolean := True;

   DMA_Periph : DMA_Peripheral renames DMA_0;

   type SCB_Index is range 0 .. 100;
   subtype Valid_SCB_Index is SCB_Index range 1 .. SCB_Index'Last;
   type SCB_Array is array (Valid_SCB_Index) of aliased DMA_Control_Block;
   type SCB_Array_Access is access all SCB_Array;

   DMA_SCB    : SCB_Array_Access := null;

   Initialized : Boolean := False;

   procedure Initialize;

   procedure DMA_Find_Free_SCB
     (Tail      : out SCB_Index;
      Available : out SCB_Index);
   --  Find the first free control block
   --  Tail: index of the last block in the current chain
   --  Available: index of the first available block

   protected DMA_Controller is
      procedure Wait_Transfer;
      --  Wait for all DMA transfers to terminate

      procedure Take_Transfer (Num_Control_Blocks : Natural;
                               Index              : out SCB_Index;
                               Status             : out Boolean);
      --  Waits for Num_Control_Blocks to be available, and returns the index
      --  of the first block. If another task is preparing a transfer, then
      --  Status is set to False

      procedure Start_Transfer;
      --  Sets the Active bit to start/resume DMA transfers

   private
      DMA_Started     : Boolean := False;
      New_Block       : SCB_Index := 0;
   end DMA_Controller;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
      Handle    : Memory_Handle;
      BUS_Addr  : BUS_Address;

      function As_SCB_Array is new Ada.Unchecked_Conversion
        (System.Address, SCB_Array_Access);

   begin
      if Initialized then
         return;
      end if;

      Memory_Allocate
        (SCB_Array'Size / 8,
         32,
         Mem_Flag_L1_Non_Allocating or
           Mem_Flag_Hint_Permalock or Mem_Flag_Zero,
         Handle);

      if Handle = Invalid_Memory_Handle then
         Put_Line ("Cannot allocate GPU memory");
      end if;

      Memory_Lock (Handle, BUS_Addr);

      if Debug then
         Put_Line ("DMA SCBs for Bitmap operations BUS addr at 0x" &
                     Image8 (UInt32 (BUS_Addr)));
      end if;

      DMA_SCB := As_SCB_Array (To_ARM (BUS_Addr));
      Initialized := DMA_SCB /= null;
   end Initialize;

   --------------------
   -- DMA_Controller --
   --------------------

   protected body DMA_Controller is

      -------------------
      -- Wait_Transfer --
      -------------------

      procedure Wait_Transfer is
      begin
         if not DMA_Started then
            return;
         end if;

         while not DMA_Periph.CS.Ended
           or else DMA_Periph.CS.Active
         loop
--              if DMA_Periph.CS.Ended then
--                 Put_Line ("ended");
--              else
--                 Put_Line ("not active");
--              end if;
            null;
         end loop;

         --  Write 1 to clear
         DMA_Periph.CS.Ended := True;

         DMA_Started := False;
      end Wait_Transfer;

      -------------------
      -- Take_Transfer --
      -------------------

      procedure Take_Transfer (Num_Control_Blocks : Natural;
                               Index              : out SCB_Index;
                               Status             : out Boolean)
      is
         Tail      : SCB_Index;
         Prev      : SCB_Index;
         Next      : SCB_Index;
      begin
         if New_Block /= 0 then
            --  Some other task is preparing a new DMA transfer. Cannot
            --  do anything until this is finished.
            Status := False;
            Index := 0;

            return;
         end if;

         if not Initialized then
            Initialize;
         end if;

         loop
            DMA_Find_Free_SCB (Tail, Index);

            if Index /= 0 then
               Prev := Index;
               --  Check that enough blocks are available
               for J in 2 .. Num_Control_Blocks loop
                  if Prev = DMA_SCB'Last then
                     Next := DMA_SCB'First;
                  else
                     Next := Prev + 1;
                  end if;

                  if DMA_Periph.CONBLK_AD =
                    To_BUS (DMA_SCB (Next)'Address)
                  then
                     --  Not enough room
                     Index := 0;
                     exit;
                  end if;
               end loop;
            end if;

            exit when Index /= 0;
         end loop;

         --  Pause the transfer
         DMA_Periph.CS.Active := False;

         --  Update the chain
         Prev := Tail;

         for J in 1 .. Num_Control_Blocks loop
            if Prev = DMA_SCB'Last then
               Next := DMA_SCB'First;
            else
               Next := Prev + 1;
            end if;

            if Prev /= 0 then
               DMA_SCB (Prev).Next_CB := To_BUS (DMA_SCB (Next)'Address);
            end if;

            Prev := Next;
         end loop;

         DMA_SCB (Next).Next_CB := 0;

         --  Setup New_Block for use when calling Start_Transfer
         New_Block := Index;
         Status    := True;
      end Take_Transfer;

      --------------------
      -- Start_Transfer --
      --------------------

      procedure Start_Transfer is
      begin
         DMA_Started := True;

         --  Prepair the peripheral for the transfer
         if DMA_Periph.CONBLK_AD = 0 then
            DMA_Periph.CONBLK_AD := To_BUS (DMA_SCB (New_Block)'Address);
         elsif DMA_Periph.NEXTCONB = 0 then
            DMA_Periph.NEXTCONB := To_BUS (DMA_SCB (New_Block)'Address);
         end if;
         --  Start/resume the transfer
         DMA_Periph.CS.Active := True;
         --  Reset New_Block
         New_Block := 0;
      end Start_Transfer;

   end DMA_Controller;

   -----------------------
   -- DMA_Find_Free_SCB --
   -----------------------

   procedure DMA_Find_Free_SCB
     (Tail      : out SCB_Index;
      Available : out SCB_Index)
   is
      Current_Block : constant BUS_Address := DMA_Periph.CONBLK_AD;
      Index         : SCB_Index;
      Next_Index    : SCB_Index;
      Transfering   : SCB_Index := 0;

   begin
      if Current_Block = 0 then
         --  Easy case: no DMA transfer is active
         Tail := 0;
         Available := DMA_SCB'First;

         return;
      end if;

      --  Transfer is currently in progress
      Index := DMA_SCB'First;
      loop
         if Index = DMA_SCB'Last then
            Next_Index := DMA_SCB'First;
         else
            Next_Index := Index + 1;
         end if;

         if Transfering = 0
           and then To_BUS (DMA_SCB (Index)'Address) = Current_Block
         then
            --  Found the block being transfered
            Transfering := Index;

         elsif Transfering /= 0 then
            --  Search for the terminal block in the chain of transfers
            if DMA_SCB (Index).Next_CB = 0 then
               --  Next block is unused
               --  But check that it's not the transfering block
               if Next_Index /= Transfering then
                  --  return the values
                  Tail := Index;
                  Available := Next_Index;
               else
                  --  all blocks are in use
                  Tail := 0;
                  Available := 0;
               end if;

               return;
            end if;
         end if;

         --  Move on to the next block
         Index := Next_Index;
      end loop;
   end DMA_Find_Free_SCB;

   -------------------
   -- Wait_Transfer --
   -------------------

   overriding procedure Wait_Transfer
     (Buffer  : RPi_Bitmap_Buffer)
   is
      pragma Unreferenced (Buffer);
   begin
      DMA_Controller.Wait_Transfer;
   end Wait_Transfer;

   ---------
   -- BPP --
   ---------

   function BPP (Buffer : RPi_Bitmap_Buffer) return Positive
   is (Bits_Per_Pixel (Buffer.Color_Mode) / 8);

   ---------------
   -- Fill_Rect --
   ---------------

   overriding procedure Fill_Rect
     (Buffer      : RPi_Bitmap_Buffer;
      Color       : Unsigned_32;
      X           : Integer;
      Y           : Integer;
      Width       : Integer;
      Height      : Integer)
   is
      Offset      : constant Storage_Offset :=
                      Storage_Offset ((X + Y * Buffer.Width) * Buffer.BPP);
      Offset2     : constant Storage_Offset :=
                      Offset +
                        Storage_Offset (Buffer.Width * Buffer.BPP);
      Dest_Stride : Integer_16;
      Status      : Boolean;
      Index       : SCB_Index;
      Num_Blocks  : Natural;
      W, H        : Natural;

   begin
      if Width <= 0 or else Height <= 0 then
         return;
      end if;

      if X + Width > Buffer.Width then
         W := Buffer.Width - X;
      else
         W := Width;
      end if;

      if Y + Height > Buffer.Height then
         H := Buffer.Height - Y;
      else
         H := Height;
      end if;

      Dest_Stride := Integer_16 ((Buffer.Width - W) * Buffer.BPP);

      if W > 1 and then H > 1 then
         Num_Blocks := 2;
      else
         Num_Blocks := 1;
      end if;

      loop
         DMA_Controller.Take_Transfer (Num_Blocks, Index, Status);
         exit when Status;
      end loop;

      if W > 1 then
         --  DMA transfers are byte-oriented, while here we need word-oriented
         --  transfers.
         --  So we need to chain two dma transfers, one repeating the color
         --  on the first line, to achieve a proper byte sequence, then
         --  the second one repeating this first line through all height
         --  First transfer fills one line

         DMA_SCB (Index).TI :=
           (Interrupt_Enable => False,
            Two_D_Mode       => True,
            Dest_Inc         => True,
            Dest_Width       => Width_128bit,
            Src_Inc          => False,
            Src_Width        => Width_32bit,
            Wait_Response    => True,
            others           => <>);
         DMA_SCB (Index).Reserved_7 := Color;
         DMA_SCB (Index).Source_Address :=
           To_BUS (DMA_SCB (Index).Reserved_7'Address);
         DMA_SCB (Index).Destination_Address := To_BUS (Buffer.Addr + Offset);
         DMA_SCB (Index).Transfer_Length :=
           (TD_Mode  => True,
            X_Length => Unsigned_16 (Buffer.BPP),
            Y_Length => UInt14 (W - 1),
            others   => <>);
         DMA_SCB (Index).Stride :=
           (S_STRIDE => 0,
            D_STRIDE => 0);

         if H > 1 then
            if Index = DMA_SCB'Last then
               Index := DMA_SCB'First;
            else
               Index := Index + 1;
            end if;

            --  Copy the first line to the following ones
            DMA_SCB (Index) :=
              (TI                  =>
                 (Interrupt_Enable => False,
                  Two_D_Mode       => True,
                  Dest_Inc         => True,
                  Dest_Width       => Width_128bit,
                  Src_Inc          => True,
                  Src_Width        => Width_128bit,
                  Wait_Response    => True,
                  others           => <>),
               Source_Address      => To_BUS (Buffer.Addr + Offset),
               Destination_Address => To_BUS (Buffer.Addr + Offset2),
               Transfer_Length     => (TD_Mode  => True,
                                       X_Length => Unsigned_16 (Buffer.BPP * W),
                                       Y_Length => UInt14 (H - 2),
                                       others   => <>),
               Stride              => (S_STRIDE => Integer_16 ((-Buffer.BPP) * W),
                                       D_STRIDE => Dest_Stride),
               Next_CB             => 0,
               others              => <>);
         end if;
      else
         DMA_SCB (Index) :=
           (TI                  =>
              (Interrupt_Enable => False,
               Two_D_Mode       => True,
               Dest_Inc         => True,
               Dest_Width       => Width_32bit,
               Src_Inc          => False,
               Src_Width        => Width_32bit,
               Wait_Response    => True,
               others           => <>),
            Source_Address      => To_BUS (DMA_SCB (Index).Reserved_7'Address),
            Destination_Address => To_BUS (Buffer.Addr + Offset),
            Transfer_Length     => (TD_Mode  => True,
                                    X_Length => Unsigned_16 (Buffer.BPP),
                                    Y_Length => UInt14 (H - 1),
                                    others   => <>),
            Stride              => (S_STRIDE => 0,
                                    D_STRIDE => Dest_Stride),
            Next_CB             => 0,
            Reserved_7          => Color,
            others              => <>);
      end if;

      --  Start the transfer
      DMA_Controller.Start_Transfer;
   end Fill_Rect;

   ---------------
   -- Copy_Rect --
   ---------------

   overriding procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      X_Src       : Natural;
      Y_Src       : Natural;
      Dst_Buffer  : RPi_Bitmap_Buffer;
      X_Dst       : Natural;
      Y_Dst       : Natural;
      Bg_Buffer   : Bitmap_Buffer'Class;
      X_Bg        : Natural;
      Y_Bg        : Natural;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
      pragma Unreferenced (X_Bg, Y_Bg);
      Src_Offset : constant Storage_Offset :=
                     Storage_Offset ((X_Src + Y_Src * Src_Buffer.Width) *
                                       Dst_Buffer.BPP);
      Src_Stride : constant Integer_16 :=
                     Integer_16 ((Src_Buffer.Width - Width) * Dst_Buffer.BPP);
      Dst_Offset : constant Storage_Offset :=
                     Storage_Offset ((X_Dst + Y_Dst * Dst_Buffer.Width) *
                                       Dst_Buffer.BPP);
      Dst_Stride : constant Integer_16 :=
                     Integer_16 ((Dst_Buffer.Width - Width) * Dst_Buffer.BPP);
      Status     : Boolean;
      Index      : SCB_Index;

   begin
      if Width = 0 or else Height = 0 then
         return;
      end if;

      if Dst_Buffer.Color_Mode /= Src_Buffer.Color_Mode then
         raise Constraint_Error with "Incompatible color modes";
      end if;

      if Bg_Buffer.Addr /= System.Null_Address then
         raise Constraint_Error with "Not implemented yet.";
      end if;

      loop
         DMA_Controller.Take_Transfer (1, Index, Status);
         exit when Status;
      end loop;

      DMA_SCB (Index) :=
        (TI                  =>
           (Interrupt_Enable => False,
            Two_D_Mode       => True,
            Wait_Response    => True,
            Dest_Inc         => True,
            Dest_Width       => Width_128bit,
            Src_Inc          => True,
            Src_Width        => Width_128bit,
            others           => <>),
         Source_Address      => To_BUS (Src_Buffer.Addr + Src_Offset),
         Destination_Address => To_BUS (Dst_Buffer.Addr + Dst_Offset),
         Transfer_Length     => (TD_Mode  => True,
                                 X_Length => Unsigned_16 (Width * Dst_Buffer.BPP),
                                 Y_Length => UInt14 (Height - 1),
                                 others   => <>),
         Stride              => (S_STRIDE => Src_Stride,
                                 D_STRIDE => Dst_Stride),
         Next_CB             => 0,
         others              => <>);

      --  Start the transfer
      DMA_Controller.Start_Transfer;

      if Synchronous then
         DMA_Controller.Wait_Transfer;
      end if;
   end Copy_Rect;

end RPi.Bitmap;
