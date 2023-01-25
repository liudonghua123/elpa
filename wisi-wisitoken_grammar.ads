--  Abstract :
--
--  Wisitoken_grammar parser language-specific runtime
--
--  Copyright (C) 2019 - 2022 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package Wisi.WisiToken_Grammar is

   Language_Protocol_Version : constant String := "1";
   --  Defines the data passed to Initialize in Params.
   --
   --  This value must match :language-protocol-version in
   --  wisitoken-grammar-mode in wisitoken-grammar-mode.el
   --
   --  Only changes once per wisitoken-grammar-mode release. Increment as
   --  soon as required, record new version in NEWS.

   type Parse_Data_Type is new Wisi.Parse_Data_Type with null record;

   overriding
   procedure Initialize (Data : in out Parse_Data_Type);

   overriding
   function Get_Token_IDs
     (User_Data    : in     Parse_Data_Type;
      Command_Line : in     String;
      Last         : in out Integer)
     return WisiToken.Token_ID_Arrays.Vector;

   procedure Check_Parens
     (Data    : in out Wisi.Parse_Data_Type'Class;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Args    : in     Arg_Index_Array);

end Wisi.WisiToken_Grammar;
