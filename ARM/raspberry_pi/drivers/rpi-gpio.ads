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

with HAL.GPIO;

with RPi.Regs.GPIO;

package RPi.GPIO is

   type GPIO_Point is limited new HAL.GPIO.GPIO_Point with record
      Pin : RPi.Regs.GPIO.GPIO_Pin;
   end record;

   type GPIO_Points is array (Positive range <>) of GPIO_Point;

   type Pin_Mode is
     (Mode_In,
      Mode_Out,
      Mode_AF0,
      Mode_AF1,
      Mode_AF2,
      Mode_AF3,
      Mode_AF4,
      Mode_AF5);

   type Pin_Resistance is
     (Open_Drain,
      Pull_Up,
      Pull_Down);

   type GPIO_Trigger is mod 2 ** 6;
   Trigger_None                      : constant GPIO_Trigger := 2#000000#;
   Trigger_Rising_Edge_Detect        : constant GPIO_Trigger := 2#000001#;
   Trigger_Falling_Edge_Detect       : constant GPIO_Trigger := 2#000010#;
   Trigger_High_Detect               : constant GPIO_Trigger := 2#000100#;
   Trigger_Low_Detect                : constant GPIO_Trigger := 2#001000#;
   Trigger_Async_Rising_Edge_Detect  : constant GPIO_Trigger := 2#010000#;
   Trigger_Async_Falling_Edge_Detect : constant GPIO_Trigger := 2#100000#;

   procedure Configure
     (This     : GPIO_Point;
      Mode     : Pin_Mode);

   procedure Configure
     (This     : GPIO_Points;
      Mode     : Pin_Mode);

   procedure Configure
     (This     : GPIO_Point;
      Resistor : Pin_Resistance);

   procedure Configure
     (This     : GPIO_Points;
      Resistor : Pin_Resistance);

   procedure Configure
     (This    : GPIO_Point;
      Trigger : GPIO_Trigger);

   overriding function Set (This : GPIO_Point) return Boolean;

   overriding procedure Set (This : in out GPIO_Point);

   overriding procedure Clear (This : in out GPIO_Point);

   overriding procedure Toggle (This : in out GPIO_Point);

   function Event_Detected (This : GPIO_Point) return Boolean;
   --  According to the Triggers configured for this Pin (see the Trigger
   --  Configure procedure), this function will tell if one of the event
   --  has been triggered.

   procedure Clear_Event (This : GPIO_Point);
   --  Clear the Event detected flag

   P0  : aliased GPIO_Point := (Pin => 0);
   P1  : aliased GPIO_Point := (Pin => 1);
   P2  : aliased GPIO_Point := (Pin => 2);
   P3  : aliased GPIO_Point := (Pin => 3);
   P4  : aliased GPIO_Point := (Pin => 4);
   P5  : aliased GPIO_Point := (Pin => 5);
   P6  : aliased GPIO_Point := (Pin => 6);
   P7  : aliased GPIO_Point := (Pin => 7);
   P8  : aliased GPIO_Point := (Pin => 8);
   P9  : aliased GPIO_Point := (Pin => 9);
   P10 : aliased GPIO_Point := (Pin => 10);
   P11 : aliased GPIO_Point := (Pin => 11);
   P12 : aliased GPIO_Point := (Pin => 12);
   P13 : aliased GPIO_Point := (Pin => 13);
   P14 : aliased GPIO_Point := (Pin => 14);
   P15 : aliased GPIO_Point := (Pin => 15);
   P16 : aliased GPIO_Point := (Pin => 16);
   P17 : aliased GPIO_Point := (Pin => 17);
   P18 : aliased GPIO_Point := (Pin => 18);
   P19 : aliased GPIO_Point := (Pin => 19);
   P20 : aliased GPIO_Point := (Pin => 20);
   P21 : aliased GPIO_Point := (Pin => 21);
   P22 : aliased GPIO_Point := (Pin => 22);
   P23 : aliased GPIO_Point := (Pin => 23);
   P24 : aliased GPIO_Point := (Pin => 24);
   P25 : aliased GPIO_Point := (Pin => 25);
   P26 : aliased GPIO_Point := (Pin => 26);
   P27 : aliased GPIO_Point := (Pin => 27);
   P28 : aliased GPIO_Point := (Pin => 28);
   P29 : aliased GPIO_Point := (Pin => 29);
   P30 : aliased GPIO_Point := (Pin => 30);
   P31 : aliased GPIO_Point := (Pin => 31);
   P32 : aliased GPIO_Point := (Pin => 32);
   P33 : aliased GPIO_Point := (Pin => 33);
   P34 : aliased GPIO_Point := (Pin => 34);
   P35 : aliased GPIO_Point := (Pin => 35);
   P36 : aliased GPIO_Point := (Pin => 36);
   P37 : aliased GPIO_Point := (Pin => 37);
   P38 : aliased GPIO_Point := (Pin => 38);
   P39 : aliased GPIO_Point := (Pin => 39);
   P40 : aliased GPIO_Point := (Pin => 40);
   P41 : aliased GPIO_Point := (Pin => 41);
   P42 : aliased GPIO_Point := (Pin => 42);
   P43 : aliased GPIO_Point := (Pin => 43);
   P44 : aliased GPIO_Point := (Pin => 44);
   P45 : aliased GPIO_Point := (Pin => 45);
   P46 : aliased GPIO_Point := (Pin => 46);
   P47 : aliased GPIO_Point := (Pin => 47);
   P48 : aliased GPIO_Point := (Pin => 48);
   P49 : aliased GPIO_Point := (Pin => 49);
   P50 : aliased GPIO_Point := (Pin => 50);
   P51 : aliased GPIO_Point := (Pin => 51);
   P52 : aliased GPIO_Point := (Pin => 52);
   P53 : aliased GPIO_Point := (Pin => 53);

end RPi.GPIO;
