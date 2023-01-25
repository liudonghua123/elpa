--  Abstract :
--
--  See spec.
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

with Wisitoken_Grammar_1_Process_Actions;
package body Wisi.WisiToken_Grammar is

   Check_Parens_Action_Index : constant String := "0";

   overriding
   procedure Initialize (Data : in out Parse_Data_Type)
   is
      use all type Wisitoken_Grammar_1_Process_Actions.Token_Enum_ID;
   begin
      Wisi.Initialize (Wisi.Parse_Data_Type (Data));

      Data.First_Comment_ID := +COMMENT_ID;

      Data.Statement_IDs.Append (+compilation_unit_list_ID);
      Data.Statement_IDs.Append (+compilation_unit_ID);
      Data.Statement_IDs.Append (+declaration_ID);
      Data.Statement_IDs.Append (+nonterminal_ID);

   end Initialize;

   overriding
   function Get_Token_IDs
     (User_Data    : in     Parse_Data_Type;
      Command_Line : in     String;
      Last         : in out Integer)
     return WisiToken.Token_ID_Arrays.Vector
   is
      pragma Unreferenced (User_Data);
      use Wisitoken_Grammar_1_Process_Actions;
   begin
      return IDs : WisiToken.Token_ID_Arrays.Vector do
         Wisi.Skip (Command_Line, Last, '(');
         loop
            IDs.Append (+Token_Enum_ID'Value (Wisi.Get_Enum (Command_Line, Last)));
            Wisi.Skip (Command_Line, Last, ' ');
            exit when Command_Line (Last + 1) = ')';
         end loop;
         Last := Last + 1;
      end return;
   end Get_Token_IDs;

   procedure Check_Parens
     (Data    : in out Wisi.Parse_Data_Type'Class;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Args    : in     Arg_Index_Array)
   is
      use WisiToken;
   begin
      for Index of Args loop
         declare
            Char_Region : constant Buffer_Region := Tree.Char_Region
              (Tree.First_Terminal (Tree.Child (Nonterm, Index)), Trailing_Non_Grammar => False);
         begin
            Data.Put_Language_Action
              (Check_Parens_Action_Index & Buffer_Pos'Image (Char_Region.First) &
                 Buffer_Pos'Image (Char_Region.Last));
         end;
      end loop;
   end Check_Parens;

end Wisi.WisiToken_Grammar;
