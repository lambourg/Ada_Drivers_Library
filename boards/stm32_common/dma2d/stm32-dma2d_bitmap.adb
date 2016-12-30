------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Cortex_M.Cache; use Cortex_M.Cache;

with STM32.DMA2D;    use STM32.DMA2D;

package body STM32.DMA2D_Bitmap is

   function To_DMA2D_CM is new Ada.Unchecked_Conversion
     (HAL.Bitmap.Bitmap_Color_Mode, STM32.DMA2D.DMA2D_Color_Mode);

   function To_DMA2D_Color is new Ada.Unchecked_Conversion
     (HAL.Bitmap.Bitmap_Color, STM32.DMA2D.DMA2D_Color);

   ---------------------
   -- To_DMA2D_Buffer --
   ---------------------

   function To_DMA2D_Buffer
     (Buffer : HAL.Bitmap.Bitmap_Buffer'Class) return DMA2D_Buffer
   is
      Color_Mode : constant DMA2D_Color_Mode :=
        To_DMA2D_CM (Buffer.Color_Mode);
      Ret : DMA2D_Buffer (Color_Mode);
   begin
      Ret.Addr := Buffer.Addr;
      Ret.Width := (if Buffer.Swapped then Buffer.Height
                             else Buffer.Width);
      Ret.Height := (if Buffer.Swapped then Buffer.Width
                     else Buffer.Height);
      return Ret;
   end To_DMA2D_Buffer;

   ---------------
   -- Set_Pixel --
   ---------------

   overriding procedure Set_Pixel
     (Buffer : DMA2D_Bitmap_Buffer;
      X      : Natural;
      Y      : Natural;
      Value  : UInt32)
   is
   begin
      DMA2D_Wait_Transfer;
      HAL.Bitmap.Bitmap_Buffer (Buffer).Set_Pixel (X, Y, Value);
   end Set_Pixel;

   ---------------------
   -- Set_Pixel_Blend --
   ---------------------

   overriding procedure Set_Pixel_Blend
     (Buffer : DMA2D_Bitmap_Buffer;
      X      : Natural;
      Y      : Natural;
      Value  : HAL.Bitmap.Bitmap_Color)
   is
      DMA_Buf : constant DMA2D_Buffer := To_DMA2D_Buffer (Buffer);
   begin
      if To_DMA2D_CM (Buffer.Color_Mode) in DMA2D_Dst_Color_Mode then
         if not Buffer.Swapped then
            DMA2D_Set_Pixel_Blend
              (Buffer => DMA_Buf,
               X      => X,
               Y      => Y,
               Color  => To_DMA2D_Color (Value));
         else
            DMA2D_Set_Pixel_Blend
              (Buffer => DMA_Buf,
               X      => Y,
               Y      => Buffer.Width - X - 1,
               Color  => To_DMA2D_Color (Value));
         end if;
      else
         HAL.Bitmap.Bitmap_Buffer (Buffer).Set_Pixel_Blend (X, Y, Value);
      end if;
   end Set_Pixel_Blend;

   ---------------
   -- Get_Pixel --
   ---------------

   overriding function Get_Pixel
     (Buffer : DMA2D_Bitmap_Buffer;
      X      : Natural;
      Y      : Natural) return UInt32
   is
   begin
      DMA2D_Wait_Transfer;
      return HAL.Bitmap.Bitmap_Buffer (Buffer).Get_Pixel (X, Y);
   end Get_Pixel;

   ----------
   -- Fill --
   ----------

   overriding procedure Fill
     (Buffer : DMA2D_Bitmap_Buffer;
      Color  : UInt32)
   is
   begin
      if To_DMA2D_CM (Buffer.Color_Mode) in DMA2D_Dst_Color_Mode then
         DMA2D_Fill (To_DMA2D_Buffer (Buffer), Color, True);
      else
         HAL.Bitmap.Bitmap_Buffer (Buffer).Fill (Color);
      end if;
   end Fill;

   ---------------
   -- Fill_Rect --
   ---------------

   overriding procedure Fill_Rect
     (Buffer : DMA2D_Bitmap_Buffer;
      Color  : UInt32;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer)
   is
      DMA_Buf : constant DMA2D_Buffer := To_DMA2D_Buffer (Buffer);
      W, H    : Integer;
   begin
      if X >= Buffer.Width or else Y >= Buffer.Height then
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


      if To_DMA2D_CM (Buffer.Color_Mode) in DMA2D_Dst_Color_Mode then
         if not Buffer.Swapped then
            DMA2D_Fill_Rect
              (DMA_Buf,
               Color  => Color,
               X      => X,
               Y      => Y,
               Width  => W,
               Height => H);
         else
            DMA2D_Fill_Rect
              (DMA_Buf,
               Color  => Color,
               X      => Y,
               Y      => Buffer.Width - X - W,
               Width  => H,
               Height => W);
         end if;
      else
         HAL.Bitmap.Bitmap_Buffer (Buffer).Fill_Rect
           (Color, X, Y, Width, Height);
      end if;
   end Fill_Rect;

   ---------------
   -- Copy_Rect --
   ---------------

   overriding procedure Copy_Rect
     (Src_Buffer  : HAL.Bitmap.Bitmap_Buffer'Class;
      X_Src       : Natural;
      Y_Src       : Natural;
      Dst_Buffer  : DMA2D_Bitmap_Buffer;
      X_Dst       : Natural;
      Y_Dst       : Natural;
      Bg_Buffer   : HAL.Bitmap.Bitmap_Buffer'Class;
      X_Bg        : Natural;
      Y_Bg        : Natural;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
      use type System.Address;
      DMA_Buf_Src : constant DMA2D_Buffer := To_DMA2D_Buffer (Src_Buffer);
      DMA_Buf_Dst : constant DMA2D_Buffer := To_DMA2D_Buffer (Dst_Buffer);
      DMA_Buf_Bg  : DMA2D_Buffer := To_DMA2D_Buffer (Bg_Buffer);
      X0_Src      : Natural := X_Src;
      Y0_Src      : Natural := Y_Src;
      X0_Dst      : Natural := X_Dst;
      Y0_Dst      : Natural := Y_Dst;
      X0_Bg       : Natural := X_Bg;
      Y0_Bg       : Natural := Y_Bg;
      W           : Natural := Width;
      H           : Natural := Height;
   begin
      if Src_Buffer.Swapped then
         X0_Src := Y_Src;
         Y0_Src := Src_Buffer.Width - X_Src - Width;
      end if;

      if Dst_Buffer.Swapped then
         X0_Dst := Y_Dst;
         Y0_Dst := Dst_Buffer.Width - X_Dst - Width;
         W := Height;
         H := Width;
      end if;

      if Bg_Buffer.Addr = System.Null_Address then
         DMA_Buf_Bg := STM32.DMA2D.Null_Buffer;
         X0_Bg := 0;
         Y0_Bg := 0;
      elsif Bg_Buffer.Swapped then
         X0_Bg := Y_Bg;
         Y0_Bg := Bg_Buffer.Width - X_Bg - Width;
      end if;

      Cortex_M.Cache.Clean_DCache (Src_Buffer.Addr, Src_Buffer.Buffer_Size);

      DMA2D_Copy_Rect
        (DMA_Buf_Src, X0_Src, Y0_Src,
         DMA_Buf_Dst, X0_Dst, Y0_Dst,
         DMA_Buf_Bg, X0_Bg, Y0_Bg,
         W, H,
         Synchronous => Synchronous);
   end Copy_Rect;

   -------------------
   -- Wait_Transfer --
   -------------------

   overriding procedure Wait_Transfer (Buffer : DMA2D_Bitmap_Buffer)
   is
   begin
      DMA2D_Wait_Transfer;
      Cortex_M.Cache.Clean_Invalidate_DCache
        (Start => Buffer.Addr,
         Len   => Buffer.Buffer_Size);
   end Wait_Transfer;

end STM32.DMA2D_Bitmap;
