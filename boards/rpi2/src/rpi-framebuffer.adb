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

pragma Ada_2012;

with Ada.Unchecked_Conversion;
with Ada.Real_Time;            use Ada.Real_Time;
with Ada.Text_IO;              use Ada.Text_IO;

with Interfaces;               use Interfaces;

with RPi.Bitmap;
with RPi.Firmware;             use RPi.Firmware;
with RPi.Regs.DMA;             use RPi.Regs.DMA;

package body RPi.Framebuffer is

   Debug : constant Boolean := True;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Display     : in out Framebuffer_Display;
      Width       : Natural;
      Height      : Natural;
      Color_Depth : Color_Depth_Type)
   is
      Alloc_Data          : Word_Array := (16, 0);
      Alloc_Data_Offset   : Natural;
      Physical_Size_Data  : constant Word_Array :=
                              (Unsigned_32 (Width), Unsigned_32 (Height));
      Virtual_Size_Data   : constant Word_Array :=
                              (Unsigned_32 (Width), Unsigned_32 (Height * 2));
      Virtual_Offset_Data : constant Word_Array := (0, 0);
      Depth_Data          : constant UInt32 := 8 * (UInt32 (Color_Depth));
      Generic_Offset      : Natural with Unreferenced;

      FB_Addr : BUS_Address;

   begin
      RPi.Firmware.Lock;

      Alloc_Data_Offset :=
        Add_Message (Tag_Allocate_Buffer, Alloc_Data);
      Generic_Offset :=
        Add_Message (Tag_Set_Physical_Display_Size, Physical_Size_Data);
      Generic_Offset :=
        Add_Message (Tag_Set_Virtual_Buffer_Size, Virtual_Size_Data);
      Generic_Offset :=
        Add_Message (Tag_Set_Virtual_Offset, Virtual_Offset_Data);
      Generic_Offset :=
        Add_Message (Tag_Set_Depth, Depth_Data);

      RPi.Firmware.Do_Transaction;

      Alloc_Data := Get_Result (Alloc_Data_Offset, 2);

      RPi.Firmware.Unlock;

      --  Map to uncached address
      FB_Addr := BUS_Address (Alloc_Data (0));
      Display.FB :=
        (1 => To_ARM (FB_Addr),
         2 => To_ARM (FB_Addr +
               BUS_Address (Width * Height * Natural (Color_Depth))));

      if Debug then
         Put ("FB BUS address: 0x");
         Put_Line (Image8 (UInt32 (FB_Addr)));
         Put ("FB size: 0x");
         Put_Line (Image8 (Alloc_Data (1)));
      end if;

      Display.Width         := Width;
      Display.Height        := Height;
      Display.Active_Buffer := 1;
      Display.Depth         := Color_Depth;

      --  Wait for screen on.
      delay until Clock + Seconds (1);
      if Debug then
         Put_Line ("Screen should  now be on...");
      end if;

      --  Enable the DMA channel used for framebuffer manipulations
      if Debug then
         Put_Line ("Enabling the DMA channel for fb purpose...");
      end if;
      DMA_Enable.Enable_0 := True;
      delay until Clock + Milliseconds (20);
      DMA_0.CS.Reset := True;
      delay until Clock + Milliseconds (200);
      DMA_0.CS :=
        (Priority       => 7,
         Panic_Priority => 7,
         Disable_Debug  => True,
         Ended          => True,
         others         => <>);
   end Initialize;

   -----------
   -- Blank --
   -----------

   procedure Blank (Display : in out Framebuffer_Display;
                    State   : Boolean)
   is
      pragma Unreferenced (Display);
   begin
      Fw_Request_RO (Tag_Blank_Screen, (if State then 1 else 0));
   end Blank;

   --------------------
   -- Set_Alpha_Mode --
   --------------------

   procedure Set_Alpha_Mode (Display : in out Framebuffer_Display;
                             Mode    : Alpha_Mode)
   is
      pragma Unreferenced (Display);
   begin
      Fw_Request_RO (Tag_Set_Alpha_Mode, Alpha_Mode'Enum_Rep (Mode));
   end Set_Alpha_Mode;

   ----------
   -- Flip --
   ----------

   procedure Flip
     (Display : in out Framebuffer_Display)
   is
      Data : Word_Array :=
               (1 => 0,
                2 => (if Display.Active_Buffer = 1
                      then Unsigned_32 (Display.Height)
                      else 0));

   begin
      Fw_Request (Tag_Set_Virtual_Offset, Data);
      Fw_Request (Tag_Set_VSync);

      if Data (2) = 0 then
         Display.Active_Buffer := 1;
      else
         Display.Active_Buffer := 2;
      end if;
   end Flip;

   ------------------------
   -- Hidden_Framebuffer --
   ------------------------

   function Hidden_Framebuffer
     (Display : Framebuffer_Display) return Bitmap_Buffer'Class
   is
      function Hidden_Buf_Num
        (Display : Framebuffer_Display) return Buffer_Type
      is (if Display.Active_Buffer = 1 then 2 else 1);

      Mode   : constant Bitmap_Color_Mode :=
                 (case Display.Depth is
                     when 1 => L_8,
                     when 2 => RGB_565,
                     when 3 => RGB_888,
                     when 4 => ARGB_8888);

      Ret    : constant RPi.Bitmap.RPi_Bitmap_Buffer :=
                 (Addr       => Display.FB (Hidden_Buf_Num (Display)),
                  Width      => Display.Width,
                  Height     => Display.Height,
                  Color_Mode => Mode,
                  Swapped    => False);
   begin
      return Ret;
   end Hidden_Framebuffer;

end RPi.Framebuffer;
