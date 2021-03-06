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
pragma Warnings (Off);
with Interfaces.Cache;         use Interfaces.Cache;
pragma Warnings (On);

with Rpi_Board;
with RPi.DMA;                  use RPi.DMA;
with RPi.Regs.DMA;             use RPi.Regs.DMA;

with Ada.Interrupts.Names;
with GNAT.IO;

package body RPi.Bitmap is

   DMA_Periph : DMA_Controller renames Rpi_Board.DMA_0;

   protected DMA_Handler is
      pragma Interrupt_Priority;

      entry Wait;

      procedure Set_Started;

   private
      procedure On_Interrupt;
      pragma Attach_Handler
        (On_Interrupt, Ada.Interrupts.Names.DMA0_Interrupt);

      No_Transfer : Boolean := True;
   end DMA_Handler;

   protected body DMA_Handler is
      ----------
      -- Wait --
      ----------

      entry Wait when No_Transfer is
      begin
         null;
      end Wait;

      -----------------
      -- Set_Started --
      -----------------

      procedure Set_Started is
      begin
         No_Transfer := False;
      end Set_Started;

      ------------------
      -- On_Interrupt --
      ------------------

      procedure On_Interrupt
      is
      begin
         --  Acknowledge the interrupt
         DMA_Periph.Device.CS.Int := True;

         if DMA_Periph.Device.CS.Error then
            if DMA_Periph.Device.DEBUG.Read_Last_Not_Set_Error then
               GNAT.IO.Put_Line ("!!! Read last not set error");
               DMA_Periph.Device.DEBUG.Read_Last_Not_Set_Error := True;
            elsif DMA_Periph.Device.DEBUG.FIFO_Error then
               GNAT.IO.Put_Line ("!!! FIFO Error");
               DMA_Periph.Device.DEBUG.FIFO_Error := True;
            elsif DMA_Periph.Device.DEBUG.Read_Error then
               GNAT.IO.Put_Line ("!!! Read Error");
               DMA_Periph.Device.DEBUG.Read_Error := True;
            else
               GNAT.IO.Put_Line ("??? UNKNOWN Error");
            end if;
         else
            No_Transfer := True;
         end if;
      end On_Interrupt;

   end DMA_Handler;

   -------------------
   -- Wait_Transfer --
   -------------------

   procedure Wait_Transfer
   is
   begin
      DMA_Handler.Wait;
   end Wait_Transfer;

   -------------------
   -- Wait_Transfer --
   -------------------

   overriding procedure Wait_Transfer
     (Buffer  : RPi_Bitmap_Buffer)
   is
      pragma Unreferenced (Buffer);
   begin
      DMA_Handler.Wait;
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
      Num_Blocks  : Natural;
      W, H        : Natural;

   begin
      GNAT.IO.Put_Line ("??? Fill_Rect is still in-progress");
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
         Take_Transfer (DMA_Periph, Num_Blocks, Status);
         exit when Status;
      end loop;

      if W > 1 then
         --  DMA transfers are byte-oriented, while here we need word-oriented
         --  transfers.
         --  So we need to chain two dma transfers, one repeating the color
         --  on the first line, to achieve a proper byte sequence, then
         --  the second one repeating this first line through all height
         --  First transfer fills one line

         Set_Control_Block
           (DMA_Periph,
            (TI =>
                 (Interrupt_Enable => True,
                  Two_D_Mode       => True,
                  Dest_Inc         => True,
                  Dest_Width       => Width_128bit,
                  Src_Inc          => False,
                  Src_Width        => Width_32bit,
                  Wait_Response    => True,
                  others           => <>),
             Destination_Address => To_BUS (Buffer.Addr + Offset),
             Transfer_Length     =>
               (TD_Mode  => True,
                X_Length => Unsigned_16 (Buffer.BPP),
                Y_Length => UInt14 (W - 1),
                others   => <>),
             Stride              =>
               (S_STRIDE => 0,
                D_STRIDE => 0),
             others              => <>),
           Color);

         if H > 1 then
            --  Copy the first line to the following ones
            Set_Control_Block
              (DMA_Periph,
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
                others              => <>));
         end if;

      else
         Set_Control_Block
           (DMA_Periph,
            (TI                  =>
                 (Interrupt_Enable => False,
                  Two_D_Mode       => True,
                  Dest_Inc         => True,
                  Dest_Width       => Width_32bit,
                  Src_Inc          => False,
                  Src_Width        => Width_32bit,
                  Wait_Response    => True,
                  others           => <>),
             Destination_Address => To_BUS (Buffer.Addr + Offset),
             Transfer_Length     => (TD_Mode  => True,
                                     X_Length => Unsigned_16 (Buffer.BPP),
                                     Y_Length => UInt14 (H - 1),
                                     others   => <>),
             Stride              => (S_STRIDE => 0,
                                     D_STRIDE => Dest_Stride),
             others              => <>),
            Color);
      end if;

      --  Start the transfer
      Start_Transfer (DMA_Periph);
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
      Synchronous : Boolean;
      Clean_Cache : Boolean := True)
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

      if Clean_Cache then
         Interfaces.Cache.Dcache_Flush_By_Range
           (Src_Buffer.Addr,
            Storage_Offset (Src_Buffer.Buffer_Size));
      end if;

      loop
         Take_Transfer (DMA_Periph, 1, Status);
         exit when Status;
      end loop;

      Set_Control_Block
        (DMA_Periph,
         (TI                  =>
              (Interrupt_Enable => True,
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
                                  X_Length => UInt16 (Width * Dst_Buffer.BPP),
                                  Y_Length => UInt14 (Height - 1),
                                  others   => <>),
          Stride              => (S_STRIDE => Src_Stride,
                                  D_STRIDE => Dst_Stride),
          others              => <>));

      --  Start the transfer
      DMA_Handler.Set_Started;
      Start_Transfer (DMA_Periph);

      if Synchronous then
         DMA_Handler.Wait;
      end if;
   end Copy_Rect;

end RPi.Bitmap;
