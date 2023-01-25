--  generated parser support file. -*- buffer-read-only:t  -*-
--  command line: wisitoken-bnf-generate.exe  --generate LR1 Ada_Emacs re2c PROCESS wisitoken_grammar_1.wy
--

--  Copyright (C) 2017 - 2022 Free Software Foundation, Inc.
--
--  Author: Stephen Leake <stephe-leake@stephe-leake.org>
--
--  This file is part of GNU Emacs.
--
--  GNU Emacs is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  GNU Emacs is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

with SAL;
with Wisi; use Wisi;
with Wisi.WisiToken_Grammar; use Wisi.WisiToken_Grammar;
package body Wisitoken_Grammar_1_Process_Actions is

   use all type Motion_Param_Array;

   procedure token_keyword_non_grammar_1
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, (1 => (3, 5, 4)));
      when Indent =>
         null;
      end case;
   end token_keyword_non_grammar_1;

   procedure token_keyword_non_grammar_2
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, (1 => (3, 5, 4)));
      when Indent =>
         null;
      end case;
   end token_keyword_non_grammar_2;

   procedure declaration_0
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
      T1 : constant SAL.Peek_Type := 1;
      T2 : constant SAL.Peek_Type := 2;
      T3 : constant SAL.Peek_Type := 3;
      T4 : constant SAL.Peek_Type := 4;
      T5 : constant SAL.Peek_Type := 5;
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         Statement_Action (Parse_Data, Tree, Nonterm, (1 => (T1, Statement_Start)));
         Name_Action (Parse_Data, Tree, Nonterm, T3);
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((T1, 5, 0), (T2, 5, 2), (T3, 5, 1)));
      when Indent =>
         Indent_Action_0 (Parse_Data, Tree, Nonterm, (T1 => (False, (Simple, (Label => None))), T2 => (False, (Simple,
         (Label => None))), T3 => (False, (Simple, (Label => None))), T4 => (False, (Hanging_0, (Int, 4), (Int, 2))),
         T5 => (False, (Hanging_0, (Int, 4), (Int, 2)))));
      end case;
   end declaration_0;

   procedure declaration_1
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
      T1 : constant SAL.Peek_Type := 1;
      T2 : constant SAL.Peek_Type := 2;
      T3 : constant SAL.Peek_Type := 3;
      T4 : constant SAL.Peek_Type := 4;
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         Statement_Action (Parse_Data, Tree, Nonterm, (1 => (T1, Statement_Start)));
         Name_Action (Parse_Data, Tree, Nonterm, T3);
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((T1, 5, 0), (T2, 5, 2), (T3, 5, 1)));
      when Indent =>
         Indent_Action_0 (Parse_Data, Tree, Nonterm, (T1 => (False, (Simple, (Label => None))), T2 => (False, (Simple,
         (Label => None))), T3 => (False, (Simple, (Label => None))), T4 => (False, (Hanging_0, (Int, 4), (Int, 2)))));
      end case;
   end declaration_1;

   procedure declaration_2
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
      T1 : constant SAL.Peek_Type := 1;
      T2 : constant SAL.Peek_Type := 2;
      T3 : constant SAL.Peek_Type := 3;
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         Statement_Action (Parse_Data, Tree, Nonterm, (1 => (T1, Statement_Start)));
         Name_Action (Parse_Data, Tree, Nonterm, T3);
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((T1, 5, 0), (T2, 5, 2), (T3, 5, 1)));
      when Indent =>
         Indent_Action_0 (Parse_Data, Tree, Nonterm, (T1 => (False, (Simple, (Label => None))), T2 => (False, (Simple,
         (Label => None))), T3 => (False, (Simple, (Label => None)))));
      end case;
   end declaration_2;

   procedure declaration_3
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((1, 5, 0), (2, 5, 2)));
      when Indent =>
         null;
      end case;
   end declaration_3;

   procedure declaration_4
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((1, 5, 0), (2, 5, 2)));
      when Indent =>
         null;
      end case;
   end declaration_4;

   procedure declaration_5
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((1, 5, 0), (2, 5, 2)));
      when Indent =>
         null;
      end case;
   end declaration_5;

   procedure declaration_6
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
      T1 : constant SAL.Peek_Type := 1;
      T2 : constant SAL.Peek_Type := 2;
      T3 : constant SAL.Peek_Type := 3;
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((T1, 5, 0), (T2, 5, 2)));
      when Indent =>
         Indent_Action_0 (Parse_Data, Tree, Nonterm, (T1 => (False, (Simple, (Label => None))), T2 => (False, (Simple,
         (Label => None))), T3 => (False, (Hanging_0, (Int, 4), (Int, 2)))));
      end case;
   end declaration_6;

   procedure declaration_7
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
      T1 : constant SAL.Peek_Type := 1;
      T2 : constant SAL.Peek_Type := 2;
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((T1, 5, 0), (T2, 5, 2)));
      when Indent =>
         Indent_Action_0 (Parse_Data, Tree, Nonterm, (T1 => (False, (Simple, (Label => None))), T2 => (False, (Simple,
         (Label => None)))));
      end case;
   end declaration_7;

   procedure declaration_8
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((1, 5, 0), (2, 5, 2)));
      when Indent =>
         null;
      end case;
   end declaration_8;

   procedure declaration_9
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((1, 5, 0), (2, 5, 2)));
      when Indent =>
         null;
      end case;
   end declaration_9;

   procedure declaration_10
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((1, 5, 0), (2, 5, 2)));
      when Indent =>
         null;
      end case;
   end declaration_10;

   procedure declaration_11
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((1, 5, 0), (2, 5, 2), (4, 5, 2)));
      when Indent =>
         null;
      end case;
   end declaration_11;

   procedure declaration_12
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((1, 5, 0), (2, 5, 2), (3, 5, 2)));
      when Indent =>
         null;
      end case;
   end declaration_12;

   procedure nonterminal_0
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
      T1 : constant SAL.Peek_Type := 1;
      T2 : constant SAL.Peek_Type := 2;
      T3 : constant SAL.Peek_Type := 3;
      T4 : constant SAL.Peek_Type := 4;
      T5 : constant SAL.Peek_Type := 5;
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         Statement_Action (Parse_Data, Tree, Nonterm, ((T1, Statement_Start), (T5, Statement_End)));
         Motion_Action (Parse_Data, Tree, Nonterm, (Index_ID'(T1, Invalid_Token_ID) & Index_ID'(T3, 56) & Index_ID'(T5,
         Invalid_Token_ID)));
         Name_Action (Parse_Data, Tree, Nonterm, T1);
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, (1 => (T1, 5, 1)));
      when Indent =>
         Indent_Action_0 (Parse_Data, Tree, Nonterm, (T1 => (True, (Simple, (Label => None)), (Simple, (Int, 2))), T2
         => (False, (Simple, (Int, 2))), T3 => (False, (Simple, (Block, 2))), T4 => (False, (Simple, (Int, 2))), T5 =>
         (False, (Simple, (Int, 2)))));
      end case;
   end nonterminal_0;

   procedure nonterminal_1
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
      T1 : constant SAL.Peek_Type := 1;
      T2 : constant SAL.Peek_Type := 2;
      T3 : constant SAL.Peek_Type := 3;
      T5 : constant SAL.Peek_Type := 4;
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         Statement_Action (Parse_Data, Tree, Nonterm, ((T1, Statement_Start), (T5, Statement_End)));
         Motion_Action (Parse_Data, Tree, Nonterm, (Index_ID'(T1, Invalid_Token_ID) & Index_ID'(T3, 56) & Index_ID'(T5,
         Invalid_Token_ID)));
         Name_Action (Parse_Data, Tree, Nonterm, T1);
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, (1 => (T1, 5, 1)));
      when Indent =>
         Indent_Action_0 (Parse_Data, Tree, Nonterm, (T1 => (True, (Simple, (Label => None)), (Simple, (Int, 2))), T2
         => (False, (Simple, (Int, 2))), T3 => (False, (Simple, (Block, 2))), T5 => (False, (Simple, (Int, 2)))));
      end case;
   end nonterminal_1;

   procedure rhs_list_0
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         Statement_Action (Parse_Data, Tree, Nonterm, (1 => (1, Motion)));
      when Face =>
         null;
      when Indent =>
         Indent_Action_0 (Parse_Data, Tree, Nonterm, (1 => (True, (Simple, (Int, 2)), (Simple, (Label => None)))));
      end case;
   end rhs_list_0;

   procedure rhs_list_1
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         Statement_Action (Parse_Data, Tree, Nonterm, (1 => (3, Motion)));
      when Face =>
         null;
      when Indent =>
         Indent_Action_0 (Parse_Data, Tree, Nonterm, ((False, (Simple, (Label => None))), (False, (Simple, (Label =>
         None))), (False, (Simple, (Int, 2)))));
      end case;
   end rhs_list_1;

   procedure rhs_list_2
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((2, 5, 0), (3, 5, 2)));
      when Indent =>
         null;
      end case;
   end rhs_list_2;

   procedure rhs_list_3
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((2, 5, 0), (3, 5, 2)));
      when Indent =>
         null;
      end case;
   end rhs_list_3;

   procedure rhs_list_4
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((2, 5, 0), (3, 5, 2)));
      when Indent =>
         null;
      end case;
   end rhs_list_4;

   procedure rhs_list_5
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((2, 5, 0), (3, 5, 2)));
      when Indent =>
         null;
      end case;
   end rhs_list_5;

   procedure rhs_list_6
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, ((2, 5, 0), (3, 5, 2), (4, 5, 2)));
      when Indent =>
         null;
      end case;
   end rhs_list_6;

   procedure rhs_0
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         null;
      when Indent =>
         Indent_Action_0 (Parse_Data, Tree, Nonterm, (1 => (True, (Hanging_0, (Label => None), (Int, 2)), (Simple,
         (Label => None)))));
      end case;
   end rhs_0;

   procedure rhs_1
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Check_Parens (Wisi.Parse_Data_Type'Class (User_Data), Tree, Nonterm, (1 => 2));
      when Indent =>
         Indent_Action_0 (Parse_Data, Tree, Nonterm, ((False, (Hanging_0, (Label => None), (Int, 2))), (False, (Simple,
         (Label => None)))));
      end case;
   end rhs_1;

   procedure rhs_2
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Check_Parens (Wisi.Parse_Data_Type'Class (User_Data), Tree, Nonterm, (2, 3));
      when Indent =>
         Indent_Action_0 (Parse_Data, Tree, Nonterm, ((False, (Hanging_0, (Label => None), (Int, 2))), (False, (Simple,
         (Label => None))), (False, (Simple, (Label => None)))));
      end case;
   end rhs_2;

   procedure rhs_item_1
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, (1 => (1, 5, 0)));
      when Indent =>
         null;
      end case;
   end rhs_item_1;

   procedure rhs_optional_item_3
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         Face_Apply_Action (Parse_Data, Tree, Nonterm, (1 => (1, 5, 0)));
      when Indent =>
         null;
      end case;
   end rhs_optional_item_3;

   procedure compilation_unit_list_0
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         null;
      when Indent =>
         Indent_Action_0 (Parse_Data, Tree, Nonterm, (1 => (True, (Simple, (Int, 0)), (Simple, (Int, 0)))));
      end case;
   end compilation_unit_list_0;

   procedure compilation_unit_list_1
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access)
   is
      Parse_Data : Wisi.Parse_Data_Type renames Wisi.Parse_Data_Type (User_Data);
   begin
      case Parse_Data.Post_Parse_Action is
      when Navigate =>
         null;
      when Face =>
         null;
      when Indent =>
         Indent_Action_0 (Parse_Data, Tree, Nonterm, ((False, (Simple, (Int, 0))), (True, (Simple, (Int, 0)), (Simple,
         (Int, 0)))));
      end case;
   end compilation_unit_list_1;

end Wisitoken_Grammar_1_Process_Actions;
