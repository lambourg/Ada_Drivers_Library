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

with System;
with Interfaces;        use Interfaces;

with HAL.Block_Drivers; use HAL.Block_Drivers;
with SDMMC_Init;        use SDMMC_Init;

package RPi.SDMMC is

   type SDCard_Driver is new SDMMC_Init.SDMMC_Driver
     and HAL.Block_Drivers.Block_Driver with null record;

   Use_DMA : Boolean := True;

   overriding procedure Delay_Milliseconds
     (This   : SDCard_Driver;
      Amount : Natural);

   overriding procedure Reset
     (This   : in out SDCard_Driver;
      Status : out SD_Error);

   overriding procedure Set_Clock
     (This : in out SDCard_Driver;
      Freq : Natural);

   overriding procedure Set_Bus_Size
     (This : in out SDCard_Driver;
      Mode : Wide_Bus_Mode);

   overriding procedure Send_Cmd
     (This : in out SDCard_Driver;
      Cmd : Cmd_Desc_Type;
      Arg : Unsigned_32;
      Status : out SD_Error);

   overriding procedure Read_Cmd
     (This : in out SDCard_Driver;
      Cmd : Cmd_Desc_Type;
      Arg : Unsigned_32;
      Buf : System.Address;
      Len : Unsigned_32;
      Status : out SD_Error);

   overriding procedure Read_Rsp48
     (This : in out SDCard_Driver;
      Rsp : out Unsigned_32);

   overriding procedure Read_Rsp136
     (This : in out SDCard_Driver;
      W0, W1, W2, W3 : out Unsigned_32);

   procedure Initialize (Driver : in out SDCard_Driver;
                         Info   : out Card_Information;
                         Status : out SD_Error);

   overriding function Read
     (Controller   : in out SDCard_Driver;
      Block_Number : Unsigned_64;
      Data         : out HAL.Block_Drivers.Block) return Boolean;
   --  Reads Data.

   overriding function Write
     (Controller   : in out SDCard_Driver;
      Block_Number : Unsigned_64;
      Data         : HAL.Block_Drivers.Block) return Boolean;
   --  Writes Data.

   --  The EMMC driver
   EMMC_Driver : aliased SDCard_Driver;

end RPi.SDMMC;
