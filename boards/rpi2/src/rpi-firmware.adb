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

with System.Storage_Elements;  use System.Storage_Elements;
with Interfaces;               use Interfaces;
pragma Warnings (Off);
with Interfaces.Cache;         use Interfaces.Cache;
pragma Warnings (On);
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with RPi.Mailbox;              use RPi.Mailbox;

------------------
-- RPi.Firmware --
------------------

package body RPi.Firmware is

   Debug              : constant Boolean := True;

   Request_Code       : constant UInt32 := 16#0#;
   Request_Indicator  : constant UInt32 := 0;
--     Response_Indicator : constant UInt32 := 16#8000_0000#;
--     Response_Success   : constant UInt32 := 16#8000_0000#;
--     Response_Error     : constant UInt32 := 16#8000_0001#;

   Message_Pool_Size  : constant := 4096;

--     Mem_Flag_Normal         : constant UInt32 := 2#000_0000#;
   --  normal allocating alias. Don't use from ARM

   Mem_Flag_Discardable    : constant UInt32 := 2#000_0001#;
   --  can be resized to 0 at any time. Use for cached data

   Mem_Flag_Direct         : constant UInt32 := 2#000_0100#;
   --  0xC alias uncached

--     Mem_Flag_Coherent       : constant UInt32 := 2#000_1000#;
   --  0x8 alias. Non-allocating in L2 but coherent

--     Mem_Flag_L1_Non_Allocating : constant UInt32 :=
--                                    Mem_Flag_Direct or Mem_Flag_Coherent;
   --  Allocating in L2

--     Mem_Flag_Zero           : constant UInt32 := 2#001_0000#;
   --  Initialise buffer to all zeros

--     Mem_Flag_No_Init        : constant UInt32 := 2#010_0000#;
   --  Don't initialize (default is initialise to ones)

   Mem_Flag_Hint_Permalock : constant UInt32 := 2#100_0000#;
   --  Likely to be locked for long periods of time.

   Initialized             : Boolean := False;

   type Message_Pool is access all Byte_Array (1 .. Message_Pool_Size);
   Pool                    : Message_Pool;
   P_Index                 : Natural := 0;

   protected Fw_Lock is
      entry Lock;
      procedure Unlock;
   private
      Unlocked : Boolean := True;
   end Fw_Lock;

   --  Kill the warning on the Pi3 that complains about mismatch between
   --  64-bit addresses and UInt32. This is expected as BUS address are 32-bit
   --  on the Pi anyway
   pragma Warnings (Off);

   function To_Unsigned_32 is new Ada.Unchecked_Conversion
     (System.Address, UInt32);
   function To_Address is new Ada.Unchecked_Conversion
     (UInt32, System.Address);

   pragma Warnings (On);

   subtype B4 is Byte_Array (1 .. 4);
   function As_Byte_Array is new Ada.Unchecked_Conversion (UInt32, B4);

   -------------
   -- Fw_Lock --
   -------------

   protected body Fw_Lock is

      ----------
      -- Lock --
      ----------

      entry Lock when Unlocked is
      begin
         Unlocked := False;
      end Lock;

      ------------
      -- Unlock --
      ------------

      procedure Unlock
      is
      begin
         Unlocked := True;
      end Unlock;
   end Fw_Lock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
      Alloc_Msg : Word_Array :=
                    (0,
                     Request_Code,

                     ARM_To_VC_Tag_Code (Tag_Allocate_Memory),
                     12,
                     Request_Indicator,
                     Message_Pool_Size, --  Size
                     16, --  Alignment
                     Mem_Flag_Direct or Mem_Flag_Hint_Permalock, --  Flags

                     0);
      Lock_Msg  : Word_Array :=
                    (0,
                     Request_Code,

                     ARM_To_VC_Tag_Code (Tag_Lock_Memory),
                     4,
                     Request_Indicator,
                     0, --  Value will hold the handle

                     0);

      Handle    : UInt32;
      BUS_Addr  : UInt32;
      Buf_Addr  : System.Address;
      Res       : UInt32 with Unreferenced;

      function As_Pool is new Ada.Unchecked_Conversion
        (System.Address, Message_Pool);

   begin
      if Initialized then
         return;
      end if;

      Alloc_Msg (0) := Alloc_Msg'Length * 4;
      Lock_Msg (0) := Lock_Msg'Length * 4;

      --  Clean and invalidate so that GPU can read it
      Dcache_Flush_By_Range (Alloc_Msg'Address, Alloc_Msg'Length * 4);
      Mailbox_Write (Alloc_Msg'Address, Property_Tags_ARM_To_VC);
      Res := Mailbox_Read (Property_Tags_ARM_To_VC);

      --  Retrieve the handle
      Handle := Alloc_Msg (5);

      Lock_Msg (5) := Handle;
      Dcache_Flush_By_Range (Lock_Msg'Address, Lock_Msg'Length * 4);
      Mailbox_Write (Lock_Msg'Address, Property_Tags_ARM_To_VC);
      Res := Mailbox_Read (Property_Tags_ARM_To_VC);

      --  Retrieve the BUS address
      BUS_Addr := Lock_Msg (5);
      Buf_Addr := To_Address (BUS_Addr and 16#3fff_ffff#);

      Pool     := As_Pool (Buf_Addr);

      if Pool /= null then
         Initialized := True;
      end if;

      if Debug then
         if Initialized then
            Put_Line ("Firmware message pool initialized at 0x" &
                        Image8 (BUS_Addr));
         else
            Put_Line ("!!! Failed to init the message pool");
         end if;
      end if;
   end Initialize;

   ------------
   -- Image8 --
   ------------

   function Image8 (V : UInt32) return String8
   is
      Res        : String8;
      Hex_Digits : constant array (0 .. 15) of Character := "0123456789abcdef";
   begin
      for I in Res'Range loop
         Res (I) := Hex_Digits (Natural (Shift_Right (V, 4 * (8 - I)) and 15));
      end loop;

      return Res;
   end Image8;

   ----------------
   -- Do_Request --
   ----------------

   procedure Fw_Request
     (Tag   : ARM_To_VC_Tag)
   is
   begin
      Lock;
      Add_Message (Tag);
      Do_Transaction;
      Unlock;
   end Fw_Request;

   ----------------
   -- Do_Request --
   ----------------

   procedure Fw_Request
     (Tag   : ARM_To_VC_Tag;
      Input : in out Byte_Array)
   is
      Offset : Natural;
   begin
      Lock;
      Offset := Add_Message (Tag, Input);
      Do_Transaction;
      Input := Get_Result (Offset, Input'Length);
      Unlock;
   end Fw_Request;

   ----------------
   -- Do_Request --
   ----------------

   procedure Fw_Request
     (Tag   : ARM_To_VC_Tag;
      Input : in out Word_Array)
   is
      BA : Byte_Array (1 .. Input'Length * 4) with Address => Input'Address;
   begin
      Fw_Request (Tag, BA);
   end Fw_Request;

   ----------------
   -- Do_Request --
   ----------------

   procedure Fw_Request
     (Tag   : ARM_To_VC_Tag;
      Input : in out UInt32)
   is
      BA : Byte_Array (1 .. 4) with Address => Input'Address;
   begin
      Fw_Request (Tag, BA);
   end Fw_Request;

   --------------------
   -- Gen_Do_Request --
   --------------------

   procedure Gen_Fw_Request
     (Tag   : ARM_To_VC_Tag;
      Input : in out T)
   is
      L : constant Natural := T'Size / 8;
      BA : Byte_Array (1 .. L) with Address => Input'Address;
   begin
      Fw_Request (Tag, BA);
   end Gen_Fw_Request;

   -------------------
   -- Do_Request_RO --
   -------------------

   procedure Fw_Request_RO
     (Tag   : ARM_To_VC_Tag;
      Input : Byte_Array)
   is
      Offset : Natural with Unreferenced;
   begin
      Lock;
      Offset := Add_Message (Tag, Input);
      Do_Transaction;
      Unlock;
   end Fw_Request_RO;

   -------------------
   -- Do_Request_RO --
   -------------------

   procedure Fw_Request_RO
     (Tag   : ARM_To_VC_Tag;
      Input : Word_Array)
   is
      BA : Byte_Array (1 .. Input'Length * 4) with Address => Input'Address;
   begin
      Fw_Request_RO (Tag, BA);
   end Fw_Request_RO;

   -------------------
   -- Do_Request_RO --
   -------------------

   procedure Fw_Request_RO
     (Tag   : ARM_To_VC_Tag;
      Input : UInt32)
   is
      BA : Byte_Array (1 .. 4) with Address => Input'Address;
   begin
      Fw_Request_RO (Tag, BA);
   end Fw_Request_RO;

   -----------------------
   -- Gen_Do_Request_RO --
   -----------------------

   procedure Gen_Fw_Request_RO
     (Tag   : ARM_To_VC_Tag;
      Input : T)
   is
      BA : Byte_Array (1 .. T'Size / 8) with Address => Input'Address;
   begin
      Fw_Request_RO (Tag, BA);
   end Gen_Fw_Request_RO;

   ----------
   -- Lock --
   ----------

   procedure Lock
   is
   begin
      Fw_Lock.Lock;

      if not Initialized then
         Initialize;
      end if;

      Pool (1 .. 4) := As_Byte_Array (0);
      Pool (5 .. 8) := As_Byte_Array (Request_Code);

      P_Index := 8;
   end Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock
   is
   begin
      Fw_Lock.UnLock;
   end Unlock;

   -----------------
   -- Add_Message --
   -----------------

   procedure Add_Message (Tag   : ARM_To_VC_Tag)
   is
   begin
      Pool (P_Index + 1 .. P_Index + 4) :=
        As_Byte_Array (ARM_To_VC_Tag_Code (Tag));
      Pool (P_Index + 5 .. P_Index + 8) := As_Byte_Array (0);
      Pool (P_Index + 9 .. P_Index + 12) := As_Byte_Array (Request_Indicator);
      P_Index := P_Index + 12;
   end Add_Message;

   -----------------
   -- Add_Message --
   -----------------

   function Add_Message
     (Tag   : ARM_To_VC_Tag;
      Input : Byte_Array) return Natural
   is
      Ret : Natural;
   begin
      Pool (P_Index + 1 .. P_Index + 4) :=
        As_Byte_Array (ARM_To_VC_Tag_Code (Tag));
      Pool (P_Index + 5 .. P_Index + 8) := As_Byte_Array (Input'Length);
      Pool (P_Index + 9 .. P_Index + 12) := As_Byte_Array (Request_Indicator);
      Ret := P_Index + 13;
      Pool (Ret .. Ret + Input'Length - 1) := Input;
      P_Index := Ret + Input'Length - 1;

      return Ret;
   end Add_Message;

   -----------------
   -- Add_Message --
   -----------------

   function Add_Message (Tag   : ARM_To_VC_Tag;
                         Input : Word_Array) return Natural
   is
      L : constant Natural := Input'Length;
      subtype WA is Word_Array (1 .. L);
      subtype BA is Byte_Array (1 .. L * 4);
      function To_BA is new Ada.Unchecked_Conversion (WA, BA);
   begin
      return Add_Message (Tag, To_BA (Input));
   end Add_Message;

   ---------------------
   -- Gen_Add_Message --
   ---------------------

   function Gen_Add_Message (Tag   : ARM_To_VC_Tag; Input : T) return Natural
   is
      L : constant Natural := T'Size / 8;
      subtype BA is Byte_Array (1 .. L);
      function To_BA is new Ada.Unchecked_Conversion (T, BA);
   begin
      return Add_Message (Tag, To_BA (Input));
   end Gen_Add_Message;

   function Word_Add_Message is new Gen_Add_Message (UInt32);

   function Add_Message
     (Tag   : ARM_To_VC_Tag;
      Input : UInt32) return Natural
      renames Word_Add_Message;

   --------------------
   -- Do_Transaction --
   --------------------

   procedure Do_Transaction
   is
      Res : Unsigned_32 with Unreferenced;
   begin
      --  Message size
      Pool (1 .. 4) := As_Byte_Array (UInt32 (P_Index + 4));
      --  Trailing 0
      Pool (P_Index + 1 .. P_Index + 4) := (others => 0);

      --  Call the mailbox
      Mailbox_Write (Pool.all'Address, Property_Tags_ARM_To_VC);
      Res := Mailbox_Read (Property_Tags_ARM_To_VC);
   end Do_Transaction;

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result (Offset : Natural; Size : Natural) return Byte_Array
   is
   begin
      return Pool (Offset .. Offset + Size - 1);
   end Get_Result;

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result (Offset : Natural; Size : Natural) return Word_Array
   is
      subtype WA is Word_Array (1 .. Size);
      subtype BA is Byte_Array (1 .. Size * 4);
      function To_WA is new Ada.Unchecked_Conversion (BA, WA);
   begin
      return To_WA (Get_Result (Offset, Size * 4));
   end Get_Result;

   --------------------
   -- Gen_Get_Result --
   --------------------

   function Gen_Get_Result (Offset : Natural) return T
   is
      L : constant Natural := T'Size / 8;
      subtype BA is Byte_Array (1 .. L);
      function To_T is new Ada.Unchecked_Conversion (BA, T);
   begin
      return To_T (Get_Result (Offset, L));
   end Gen_Get_Result;

   ---------------------
   -- Word_Get_Result --
   ---------------------

   function Word_Get_Result is new Gen_Get_Result (UInt32);

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result (Offset : Natural) return UInt32
     renames Word_Get_Result;

end RPi.Firmware;
