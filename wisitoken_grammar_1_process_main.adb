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
with WisiToken.Lexer.re2c;
with wisitoken_grammar_1_re2c_c;
with Wisitoken_Grammar_1_Process_Actions; use Wisitoken_Grammar_1_Process_Actions;
package body Wisitoken_Grammar_1_Process_Main is

   function Is_Block_Delimited (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when
         COMMENT_ID |
         RAW_CODE_ID |
         REGEXP_ID |
         ACTION_ID |
         STRING_LITERAL_1_ID |
         STRING_LITERAL_2_ID => return True;
      when others => return False;
      end case;
   end Is_Block_Delimited;

   function Same_Block_Delimiters (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when COMMENT_ID => return False;
      when RAW_CODE_ID => return False;
      when REGEXP_ID => return False;
      when ACTION_ID => return False;
      when STRING_LITERAL_1_ID => return True;
      when STRING_LITERAL_2_ID => return True;
      when others => return False;
      end case;
   end Same_Block_Delimiters;

   function Escape_Delimiter_Doubled (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when others => return False;
      end case;
   end Escape_Delimiter_Doubled;

   function Start_Delimiter_Length (ID : in WisiToken.Token_ID) return Integer
   is begin
      case To_Token_Enum (ID) is
      when COMMENT_ID => return 2;
      when RAW_CODE_ID => return 2;
      when REGEXP_ID => return 2;
      when ACTION_ID => return 2;
      when STRING_LITERAL_1_ID => return 1;
      when STRING_LITERAL_2_ID => return 1;
      when others => raise SAL.Programmer_Error; return 0;
      end case;
   end Start_Delimiter_Length;

   function End_Delimiter_Length (ID : in WisiToken.Token_ID) return Integer
   is begin
      case To_Token_Enum (ID) is
      when
         COMMENT_ID |
         STRING_LITERAL_1_ID |
         STRING_LITERAL_2_ID => return 1;
      when RAW_CODE_ID => return 2;
      when REGEXP_ID => return 2;
      when ACTION_ID => return 2;
      when others => raise SAL.Programmer_Error; return 0;
      end case;
   end End_Delimiter_Length;

   function New_Line_Is_End_Delimiter (ID : in WisiToken.Token_ID) return Boolean
   is begin
      return
        (case To_Token_Enum (ID) is
         when COMMENT_ID => True,
         when RAW_CODE_ID => False,
         when REGEXP_ID => False,
         when ACTION_ID => False,
         when STRING_LITERAL_1_ID => True,
         when STRING_LITERAL_2_ID => True,
         when others => raise SAL.Programmer_Error);
   end New_Line_Is_End_Delimiter;

   function Find_End_Delimiter
     (Source      : in WisiToken.Lexer.Source;
      ID          : in WisiToken.Token_ID;
      Token_Start : in WisiToken.Buffer_Pos)
     return WisiToken.Buffer_Pos
   is begin
      return
        (case To_Token_Enum (ID) is
         when COMMENT_ID => WisiToken.Lexer.Find_New_Line (Source, Token_Start),
         when RAW_CODE_ID => WisiToken.Lexer.Find_String (Source, Token_Start, "}%"),
         when REGEXP_ID => WisiToken.Lexer.Find_String (Source, Token_Start, "]%"),
         when ACTION_ID => WisiToken.Lexer.Find_String (Source, Token_Start, ")%"),
         when STRING_LITERAL_1_ID => WisiToken.Lexer.Find_String_Or_New_Line (Source, Token_Start, """"),
         when STRING_LITERAL_2_ID => WisiToken.Lexer.Find_String_Or_New_Line (Source, Token_Start, """"),
         when others => raise SAL.Programmer_Error);
   end Find_End_Delimiter;

   function Find_Scan_End
     (Source   : in WisiToken.Lexer.Source;
      ID       : in WisiToken.Token_ID;
      Region   : in WisiToken.Buffer_Region;
      Inserted : in Boolean;
      Start    : in Boolean)
     return WisiToken.Buffer_Pos
   is
      use WisiToken;
   begin
      return
        (case To_Token_Enum (ID) is
         when COMMENT_ID =>
         (if Inserted then Region.Last
          elsif Start then Region.Last
          else Lexer.Find_New_Line (Source, Region.Last)),
         when RAW_CODE_ID =>
         (if Inserted then Region.Last
          elsif Start then Region.Last
          else Lexer.Find_String (Source, Region.First, "}%")),
         when REGEXP_ID =>
         (if Inserted then Region.Last
          elsif Start then Region.Last
          else Lexer.Find_String (Source, Region.First, "]%")),
         when ACTION_ID =>
         (if Inserted then Region.Last
          elsif Start then Region.Last
          else Lexer.Find_String (Source, Region.First, ")%")),
         when STRING_LITERAL_1_ID => Lexer.Find_New_Line (Source, Region.Last),
         when STRING_LITERAL_2_ID => Lexer.Find_New_Line (Source, Region.Last),
         when others => raise SAL.Programmer_Error);
   end Find_Scan_End;

   function Contains_End_Delimiter
     (Source : in WisiToken.Lexer.Source;
      ID     : in WisiToken.Token_ID;
      Region : in WisiToken.Buffer_Region)
     return WisiToken.Base_Buffer_Pos
   is
      use WisiToken;
   begin
      return
        (case To_Token_Enum (ID) is
         when COMMENT_ID => Lexer.Find_New_Line (Source, Region),
         when RAW_CODE_ID => Lexer.Find_String_Or_New_Line (Source, Region, "}%"),
         when REGEXP_ID => Lexer.Find_String_Or_New_Line (Source, Region, "]%"),
         when ACTION_ID => Lexer.Find_String_Or_New_Line (Source, Region, ")%"),
         when STRING_LITERAL_1_ID => Lexer.Find_String_Or_New_Line (Source, Region, """"),
         when STRING_LITERAL_2_ID => Lexer.Find_String_Or_New_Line (Source, Region, "'"),
         when others => raise SAL.Programmer_Error);
   end Contains_End_Delimiter;

   function Line_Begin_Char_Pos
    (Source : in WisiToken.Lexer.Source;
     Token  : in WisiToken.Lexer.Token;
     Line   : in WisiToken.Line_Number_Type)
   return WisiToken.Buffer_Pos
   is
      use all type WisiToken.Base_Buffer_Pos;
   begin
      case To_Token_Enum (Token.ID) is
      when NEW_LINE_ID => return Token.Char_Region.Last + 1;
      when COMMENT_ID => return Token.Char_Region.Last + 1;
      when RAW_CODE_ID => return WisiToken.Lexer.Line_Begin_Char_Pos (Source, Token, Line);
      when REGEXP_ID => return WisiToken.Lexer.Line_Begin_Char_Pos (Source, Token, Line);
      when ACTION_ID => return WisiToken.Lexer.Line_Begin_Char_Pos (Source, Token, Line);
      when others => raise SAL.Programmer_Error;
      end case;
   end Line_Begin_Char_Pos;

   function Can_Contain_New_Line (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when NEW_LINE_ID => return True;
      when COMMENT_ID => return True;
      when RAW_CODE_ID => return True;
      when REGEXP_ID => return True;
      when ACTION_ID => return True;
      when others => return False;
      end case;
   end Can_Contain_New_Line;

   function Terminated_By_New_Line (ID : in WisiToken.Token_ID) return Boolean
   is begin
      case To_Token_Enum (ID) is
      when NEW_LINE_ID => return True;
      when COMMENT_ID => return True;
      when STRING_LITERAL_1_ID => return True;
      when STRING_LITERAL_2_ID => return True;
      when others => return False;
      end case;
   end Terminated_By_New_Line;

   package Lexer is new WisiToken.Lexer.re2c
     (wisitoken_grammar_1_re2c_c.New_Lexer,
      wisitoken_grammar_1_re2c_c.Free_Lexer,
      wisitoken_grammar_1_re2c_c.Reset_Lexer,
      wisitoken_grammar_1_re2c_c.Set_Verbosity,
      wisitoken_grammar_1_re2c_c.Set_Position,
      wisitoken_grammar_1_re2c_c.Next_Token,
      Is_Block_Delimited,
      Same_Block_Delimiters,
      Escape_Delimiter_Doubled,
      Start_Delimiter_Length,
      End_Delimiter_Length,
      New_Line_Is_End_Delimiter,
      Find_End_Delimiter,
      Contains_End_Delimiter,
      Find_Scan_End,
      Line_Begin_Char_Pos,
      Can_Contain_New_Line,
      Terminated_By_New_Line);

   function Create_Parse_Table
     return WisiToken.Parse.LR.Parse_Table_Ptr
   is
      use WisiToken.Parse.LR;
      McKenzie_Param : constant McKenzie_Param_Type :=
        (First_Terminal    => 3,
         Last_Terminal     => 42,
         First_Nonterminal => 43,
         Last_Nonterminal  => 66,
         Insert =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2,
            2, 2, 2, 2),
         Delete =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2),
         Push_Back =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Undo_Reduce =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Minimal_Complete_Cost_Delta => -1,
         Fast_Forward =>  0,
         Matching_Begin =>  0,
         Ignore_Check_Fail  => 2,
         Check_Limit => 4,
         Zombie_Limit => 3,
         Check_Delta_Limit => 2147483647,
         Enqueue_Limit => 10000);

      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 255,
         First_Terminal    => 3,
         Last_Terminal     => 42,
         First_Nonterminal => 43,
         Last_Nonterminal  => 66);
   begin
      declare
         procedure Subr_1
         is begin
            Table.States (0).Action_List.Set_Capacity (2);
            Add_Action (Table.States (0), 30, (48, 0), 1);
            Add_Action (Table.States (0), 39, (54, 0), 2);
            Table.States (0).Goto_List.Set_Capacity (4);
            Add_Goto (Table.States (0), 48, 3);
            Add_Goto (Table.States (0), 54, 4);
            Add_Goto (Table.States (0), 65, 5);
            Add_Goto (Table.States (0), 66, 6);
            Table.States (1).Action_List.Set_Capacity (10);
            Add_Action (Table.States (1), 4, (48, 3), 7);
            Add_Action (Table.States (1), 5, (48, 4), 8);
            Add_Action (Table.States (1), 6, (48, 5), 9);
            Add_Action (Table.States (1), 7, (48, 12), 10);
            Add_Action (Table.States (1), 8, (48, 10), 11);
            Add_Action (Table.States (1), 9, (48, 8), 12);
            Add_Action (Table.States (1), 11, (47, 0), 13);
            Add_Action (Table.States (1), 12, (47, 1), 14);
            Add_Action (Table.States (1), 16, (47, 2), 15);
            Add_Action (Table.States (1), 39, (48, 6), 16);
            Table.States (1).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (1), 47, 17);
            Table.States (1).Kernel := To_Vector ((((48, 0),  30,  4, (32767, 0),  0), ((48, 1),  30,  3, (32767, 0),
            0), ((48, 2),  30,  2, (32767, 0),  0), ((48, 3),  30,  3, (32767, 0),  0), ((48, 4),  30,  5, (32767, 0),
            0), ((48, 5),  30,  6, (32767, 0),  0), ((48, 6),  30,  2, (32767, 0),  0), ((48, 7),  30,  1, (32767, 0),
            0), ((48, 8),  30,  4, (32767, 0),  0), ((48, 9),  30,  4, (32767, 0),  0), ((48, 10),  30,  4, (32767, 0),
             0), ((48, 11),  30,  4, (32767, 0),  0), ((48, 12),  30,  2, (32767, 0),  0)));
            Table.States (1).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 6),  39, 16)));
            Table.States (2).Action_List.Set_Capacity (2);
            Add_Action (Table.States (2), 21, (53, 0), 18);
            Add_Action (Table.States (2), 22, (53, 1), 19);
            Table.States (2).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (2), 53, 20);
            Table.States (2).Kernel := To_Vector ((((54, 0),  39,  4, (32767, 0),  0), ((54, 1),  39,  3, (32767, 0),
            0)));
            Table.States (2).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (53, 0),  21, 18)));
            Table.States (3).Action_List.Set_Capacity (3);
            Add_Action (Table.States (3), (30, 39, 42), (65, 0),  1);
            Table.States (3).Kernel := To_Vector ((0 => ((65, 0),  48,  0, (65, 0),  1)));
            Table.States (3).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (65, 0),  1)));
            Table.States (4).Action_List.Set_Capacity (3);
            Add_Action (Table.States (4), (30, 39, 42), (65, 1),  1);
            Table.States (4).Kernel := To_Vector ((0 => ((65, 1),  54,  0, (65, 1),  1)));
            Table.States (4).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (65, 1),  1)));
            Table.States (5).Action_List.Set_Capacity (3);
            Add_Action (Table.States (5), (30, 39, 42), (66, 0),  1);
            Table.States (5).Kernel := To_Vector ((0 => ((66, 0),  65,  0, (66, 0),  1)));
            Table.States (5).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (66, 0),  1)));
            Table.States (6).Action_List.Set_Capacity (3);
            Add_Action (Table.States (6), 30, (48, 0), 1);
            Add_Action (Table.States (6), 39, (54, 0), 2);
            Add_Action (Table.States (6), 42, Accept_It, (43, 0),  1);
            Table.States (6).Goto_List.Set_Capacity (3);
            Add_Goto (Table.States (6), 48, 3);
            Add_Goto (Table.States (6), 54, 4);
            Add_Goto (Table.States (6), 65, 21);
            Table.States (7).Action_List.Set_Capacity (1);
            Add_Action (Table.States (7), 39, (49, 0), 22);
            Table.States (7).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (7), 49, 23);
            Table.States (7).Kernel := To_Vector ((0 => ((48, 3),  4,  2, (32767, 0),  0)));
            Table.States (7).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (49, 0),  39, 22)));
            Table.States (8).Action_List.Set_Capacity (4);
            Add_Action (Table.States (8), 3, (45, 2), 24);
            Add_Action (Table.States (8), 14, (45, 1), 25);
            Add_Action (Table.States (8), 15, (45, 0), 26);
            Add_Action (Table.States (8), 39, (45, 3), 27);
            Table.States (8).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (8), 45, 28);
            Add_Goto (Table.States (8), 46, 29);
            Table.States (8).Kernel := To_Vector ((0 => ((48, 4),  5,  4, (32767, 0),  0)));
            Table.States (8).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (45, 3),  39, 27)));
            Table.States (9).Action_List.Set_Capacity (4);
            Add_Action (Table.States (9), 3, (45, 2), 24);
            Add_Action (Table.States (9), 14, (45, 1), 25);
            Add_Action (Table.States (9), 15, (45, 0), 26);
            Add_Action (Table.States (9), 39, (45, 3), 27);
            Table.States (9).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (9), 45, 28);
            Add_Goto (Table.States (9), 46, 30);
            Table.States (9).Kernel := To_Vector ((0 => ((48, 5),  6,  5, (32767, 0),  0)));
            Table.States (9).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (45, 3),  39, 27)));
            Table.States (10).Action_List.Set_Capacity (1);
            Add_Action (Table.States (10), 9, (48, 12), 31);
            Table.States (10).Kernel := To_Vector ((0 => ((48, 12),  7,  1, (32767, 0),  0)));
            Table.States (10).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 12),  9, 31)));
            Table.States (11).Action_List.Set_Capacity (1);
            Add_Action (Table.States (11), 39, (48, 10), 32);
            Table.States (11).Kernel := To_Vector ((((48, 10),  8,  3, (32767, 0),  0), ((48, 11),  8,  3, (32767, 0),
            0)));
            Table.States (11).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 10),  39, 32)));
            Table.States (12).Action_List.Set_Capacity (1);
            Add_Action (Table.States (12), 39, (48, 8), 33);
            Table.States (12).Kernel := To_Vector ((((48, 8),  9,  3, (32767, 0),  0), ((48, 9),  9,  3, (32767, 0),
            0)));
            Table.States (12).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 8),  39, 33)));
            Table.States (13).Action_List.Set_Capacity (1);
            Add_Action (Table.States (13), (1 =>  39), (47, 0),  1);
            Table.States (13).Kernel := To_Vector ((0 => ((47, 0),  11,  0, (47, 0),  1)));
            Table.States (13).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (47, 0),  1)));
            Table.States (14).Action_List.Set_Capacity (1);
            Add_Action (Table.States (14), 28, (47, 1), 34);
            Table.States (14).Kernel := To_Vector ((0 => ((47, 1),  12,  3, (32767, 0),  0)));
            Table.States (14).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 1),  28, 34)));
            Table.States (15).Action_List.Set_Capacity (1);
            Add_Action (Table.States (15), 28, (47, 2), 35);
            Table.States (15).Kernel := To_Vector ((0 => ((47, 2),  16,  3, (32767, 0),  0)));
            Table.States (15).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 2),  28, 35)));
            Table.States (16).Action_List.Set_Capacity (7);
            Add_Action (Table.States (16), 18, (44, 0), 36);
            Add_Action (Table.States (16), 30, Reduce, (48, 7),  2);
            Add_Action (Table.States (16), 38, (51, 1), 37);
            Add_Action (Table.States (16), 39, (51, 0), 38);
            Add_Conflict (Table.States (16), 39, (48, 7),  2);
            Add_Action (Table.States (16), 40, (44, 1), 39);
            Add_Action (Table.States (16), 41, (44, 2), 40);
            Add_Action (Table.States (16), 42, Reduce, (48, 7),  2);
            Table.States (16).Goto_List.Set_Capacity (3);
            Add_Goto (Table.States (16), 44, 41);
            Add_Goto (Table.States (16), 51, 42);
            Add_Goto (Table.States (16), 52, 43);
            Table.States (16).Kernel := To_Vector ((((48, 6),  39,  1, (32767, 0),  0), ((48, 7),  39,  0, (48, 7),
            2)));
            Table.States (16).Minimal_Complete_Actions := To_Vector (((Shift, (51, 0),  39, 38), (Reduce, (48, 7),
            2)));
            Table.States (17).Action_List.Set_Capacity (1);
            Add_Action (Table.States (17), 39, (48, 0), 44);
            Table.States (17).Kernel := To_Vector ((((48, 0),  47,  3, (32767, 0),  0), ((48, 1),  47,  2, (32767, 0),
            0), ((48, 2),  47,  1, (32767, 0),  0)));
            Table.States (17).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 0),  39, 44)));
            Table.States (18).Action_List.Set_Capacity (6);
            Add_Action (Table.States (18), (25, 26, 27, 28, 39, 41), (53, 0),  1);
            Table.States (18).Kernel := To_Vector ((0 => ((53, 0),  21,  0, (53, 0),  1)));
            Table.States (18).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 0),  1)));
            Table.States (19).Action_List.Set_Capacity (6);
            Add_Action (Table.States (19), (25, 26, 27, 28, 39, 41), (53, 1),  1);
            Table.States (19).Kernel := To_Vector ((0 => ((53, 1),  22,  0, (53, 1),  1)));
            Table.States (19).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 1),  1)));
            Table.States (20).Action_List.Set_Capacity (6);
            Add_Action (Table.States (20), 25, (63, 0), 45);
            Add_Action (Table.States (20), 26, (62, 0), 46);
            Add_Action (Table.States (20), 27, (61, 0), 47);
            Add_Action (Table.States (20), 28, (57, 0), 48);
            Add_Action (Table.States (20), 39, (58, 1), 49);
            Add_Action (Table.States (20), 41, (60, 1), 50);
            Table.States (20).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (20), 55, 51);
            Add_Goto (Table.States (20), 56, 52);
            Add_Goto (Table.States (20), 57, 53);
            Add_Goto (Table.States (20), 58, 54);
            Add_Goto (Table.States (20), 59, 55);
            Add_Goto (Table.States (20), 60, 56);
            Add_Goto (Table.States (20), 61, 57);
            Add_Goto (Table.States (20), 62, 58);
            Add_Goto (Table.States (20), 63, 59);
            Table.States (20).Kernel := To_Vector ((((54, 0),  53,  3, (32767, 0),  0), ((54, 1),  53,  2, (32767, 0),
            0)));
            Table.States (20).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 49)));
            Table.States (21).Action_List.Set_Capacity (3);
            Add_Action (Table.States (21), (30, 39, 42), (66, 1),  2);
            Table.States (21).Kernel := To_Vector ((0 => ((66, 1),  65,  0, (66, 1),  2)));
            Table.States (21).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (66, 1),  2)));
            Table.States (22).Action_List.Set_Capacity (2);
            Add_Action (Table.States (22), (17, 39), (49, 0),  1);
            Table.States (22).Kernel := To_Vector ((0 => ((49, 0),  39,  0, (49, 0),  1)));
            Table.States (22).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 0),  1)));
            Table.States (23).Action_List.Set_Capacity (2);
            Add_Action (Table.States (23), 17, (48, 3), 60);
            Add_Action (Table.States (23), 39, (49, 1), 61);
            Table.States (23).Kernel := To_Vector ((((48, 3),  49,  1, (32767, 0),  0), ((49, 1),  49,  1, (32767, 0),
            0)));
            Table.States (23).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 3),  17, 60)));
            Table.States (24).Action_List.Set_Capacity (1);
            Add_Action (Table.States (24), 39, (45, 2), 62);
            Table.States (24).Kernel := To_Vector ((0 => ((45, 2),  3,  1, (32767, 0),  0)));
            Table.States (24).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (45, 2),  39, 62)));
            Table.States (25).Action_List.Set_Capacity (1);
            Add_Action (Table.States (25), 39, (45, 1), 63);
            Table.States (25).Kernel := To_Vector ((0 => ((45, 1),  14,  1, (32767, 0),  0)));
            Table.States (25).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (45, 1),  39, 63)));
            Table.States (26).Action_List.Set_Capacity (1);
            Add_Action (Table.States (26), 39, (45, 0), 64);
            Table.States (26).Kernel := To_Vector ((0 => ((45, 0),  15,  1, (32767, 0),  0)));
            Table.States (26).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (45, 0),  39, 64)));
            Table.States (27).Action_List.Set_Capacity (2);
            Add_Action (Table.States (27), (13, 20), (45, 3),  1);
            Table.States (27).Kernel := To_Vector ((0 => ((45, 3),  39,  0, (45, 3),  1)));
            Table.States (27).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 3),  1)));
            Table.States (28).Action_List.Set_Capacity (2);
            Add_Action (Table.States (28), (13, 20), (46, 0),  1);
            Table.States (28).Kernel := To_Vector ((0 => ((46, 0),  45,  0, (46, 0),  1)));
            Table.States (28).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 0),  1)));
            Table.States (29).Action_List.Set_Capacity (2);
            Add_Action (Table.States (29), 13, (48, 4), 65);
            Add_Action (Table.States (29), 20, (46, 1), 66);
            Table.States (29).Kernel := To_Vector ((((46, 1),  46,  2, (32767, 0),  0), ((46, 2),  46,  2, (32767, 0),
            0), ((48, 4),  46,  3, (32767, 0),  0)));
            Table.States (29).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 4),  13, 65)));
            Table.States (30).Action_List.Set_Capacity (2);
            Add_Action (Table.States (30), 13, (48, 5), 67);
            Add_Action (Table.States (30), 20, (46, 1), 66);
            Table.States (30).Kernel := To_Vector ((((46, 1),  46,  2, (32767, 0),  0), ((46, 2),  46,  2, (32767, 0),
            0), ((48, 5),  46,  4, (32767, 0),  0)));
            Table.States (30).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 5),  13, 67)));
            Table.States (31).Action_List.Set_Capacity (3);
            Add_Action (Table.States (31), (30, 39, 42), (48, 12),  3);
            Table.States (31).Kernel := To_Vector ((0 => ((48, 12),  9,  0, (48, 12),  3)));
            Table.States (31).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 12),  3)));
            Table.States (32).Action_List.Set_Capacity (2);
            Add_Action (Table.States (32), 10, (48, 11), 68);
            Add_Action (Table.States (32), 23, (48, 10), 69);
            Table.States (32).Kernel := To_Vector ((((48, 10),  39,  2, (32767, 0),  0), ((48, 11),  39,  2, (32767,
            0),  0)));
            Table.States (32).Minimal_Complete_Actions := To_Vector (((Shift, (48, 10),  23, 69), (Shift, (48, 11),
            10, 68)));
            Table.States (33).Action_List.Set_Capacity (2);
            Add_Action (Table.States (33), 10, (48, 9), 70);
            Add_Action (Table.States (33), 23, (48, 8), 71);
            Table.States (33).Kernel := To_Vector ((((48, 8),  39,  2, (32767, 0),  0), ((48, 9),  39,  2, (32767, 0),
            0)));
            Table.States (33).Minimal_Complete_Actions := To_Vector (((Shift, (48, 8),  23, 71), (Shift, (48, 9),  10,
            70)));
            Table.States (34).Action_List.Set_Capacity (1);
            Add_Action (Table.States (34), 39, (47, 1), 72);
            Table.States (34).Kernel := To_Vector ((0 => ((47, 1),  28,  2, (32767, 0),  0)));
            Table.States (34).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 1),  39, 72)));
            Table.States (35).Action_List.Set_Capacity (1);
            Add_Action (Table.States (35), 39, (47, 2), 73);
            Table.States (35).Kernel := To_Vector ((0 => ((47, 2),  28,  2, (32767, 0),  0)));
            Table.States (35).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 2),  39, 73)));
            Table.States (36).Action_List.Set_Capacity (7);
            Add_Action (Table.States (36), (18, 30, 38, 39, 40, 41, 42), (44, 0),  1);
            Table.States (36).Kernel := To_Vector ((0 => ((44, 0),  18,  0, (44, 0),  1)));
            Table.States (36).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 0),  1)));
            Table.States (37).Action_List.Set_Capacity (7);
            Add_Action (Table.States (37), (18, 30, 38, 39, 40, 41, 42), (51, 1),  1);
            Table.States (37).Kernel := To_Vector ((0 => ((51, 1),  38,  0, (51, 1),  1)));
            Table.States (37).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 1),  1)));
            Table.States (38).Action_List.Set_Capacity (7);
            Add_Action (Table.States (38), (18, 30, 38, 39, 40, 41, 42), (51, 0),  1);
            Table.States (38).Kernel := To_Vector ((0 => ((51, 0),  39,  0, (51, 0),  1)));
            Table.States (38).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 0),  1)));
            Table.States (39).Action_List.Set_Capacity (7);
            Add_Action (Table.States (39), (18, 30, 38, 39, 40, 41, 42), (44, 1),  1);
            Table.States (39).Kernel := To_Vector ((0 => ((44, 1),  40,  0, (44, 1),  1)));
            Table.States (39).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 1),  1)));
            Table.States (40).Action_List.Set_Capacity (7);
            Add_Action (Table.States (40), (18, 30, 38, 39, 40, 41, 42), (44, 2),  1);
            Table.States (40).Kernel := To_Vector ((0 => ((44, 2),  41,  0, (44, 2),  1)));
            Table.States (40).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 2),  1)));
            Table.States (41).Action_List.Set_Capacity (7);
            Add_Action (Table.States (41), (18, 30, 38, 39, 40, 41, 42), (51, 2),  1);
            Table.States (41).Kernel := To_Vector ((0 => ((51, 2),  44,  0, (51, 2),  1)));
            Table.States (41).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (51, 2),  1)));
            Table.States (42).Action_List.Set_Capacity (7);
            Add_Action (Table.States (42), (18, 30, 38, 39, 40, 41, 42), (52, 0),  1);
            Table.States (42).Kernel := To_Vector ((0 => ((52, 0),  51,  0, (52, 0),  1)));
            Table.States (42).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 0),  1)));
            Table.States (43).Action_List.Set_Capacity (7);
            Add_Action (Table.States (43), 18, (44, 0), 36);
            Add_Action (Table.States (43), 30, Reduce, (48, 6),  3);
            Add_Action (Table.States (43), 38, (51, 1), 37);
            Add_Action (Table.States (43), 39, (51, 0), 38);
            Add_Conflict (Table.States (43), 39, (48, 6),  3);
            Add_Action (Table.States (43), 40, (44, 1), 39);
            Add_Action (Table.States (43), 41, (44, 2), 40);
            Add_Action (Table.States (43), 42, Reduce, (48, 6),  3);
            Table.States (43).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (43), 44, 41);
            Add_Goto (Table.States (43), 51, 74);
            Table.States (43).Kernel := To_Vector ((((48, 6),  52,  0, (48, 6),  3), ((52, 1),  52,  1, (32767, 0),
            0)));
            Table.States (43).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 6),  3)));
            Table.States (44).Action_List.Set_Capacity (6);
            Add_Action (Table.States (44), 18, (44, 0), 75);
            Add_Action (Table.States (44), 30, Reduce, (48, 2),  3);
            Add_Action (Table.States (44), 39, Reduce, (48, 2),  3);
            Add_Action (Table.States (44), 40, (44, 1), 76);
            Add_Action (Table.States (44), 41, (44, 2), 77);
            Add_Action (Table.States (44), 42, Reduce, (48, 2),  3);
            Table.States (44).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (44), 44, 78);
            Table.States (44).Kernel := To_Vector ((((48, 0),  39,  2, (32767, 0),  0), ((48, 1),  39,  1, (32767, 0),
            0), ((48, 2),  39,  0, (48, 2),  3)));
            Table.States (44).Minimal_Complete_Actions := To_Vector (((Shift, (44, 0),  18, 75), (Reduce, (48, 2),
            3)));
            Table.States (45).Action_List.Set_Capacity (6);
            Add_Action (Table.States (45), 25, (63, 0), 79);
            Add_Action (Table.States (45), 26, (62, 0), 80);
            Add_Action (Table.States (45), 27, (61, 0), 81);
            Add_Action (Table.States (45), 28, (57, 0), 82);
            Add_Action (Table.States (45), 39, (58, 1), 83);
            Add_Action (Table.States (45), 41, (60, 1), 84);
            Table.States (45).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (45), 57, 85);
            Add_Goto (Table.States (45), 58, 86);
            Add_Goto (Table.States (45), 59, 87);
            Add_Goto (Table.States (45), 60, 88);
            Add_Goto (Table.States (45), 61, 89);
            Add_Goto (Table.States (45), 62, 90);
            Add_Goto (Table.States (45), 63, 91);
            Add_Goto (Table.States (45), 64, 92);
            Table.States (45).Kernel := To_Vector ((((63, 0),  25,  2, (32767, 0),  0), ((63, 1),  25,  3, (32767, 0),
            0)));
            Table.States (45).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 83)));
            Table.States (46).Action_List.Set_Capacity (6);
            Add_Action (Table.States (46), 25, (63, 0), 93);
            Add_Action (Table.States (46), 26, (62, 0), 94);
            Add_Action (Table.States (46), 27, (61, 0), 95);
            Add_Action (Table.States (46), 28, (57, 0), 96);
            Add_Action (Table.States (46), 39, (58, 1), 97);
            Add_Action (Table.States (46), 41, (60, 1), 98);
            Table.States (46).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (46), 57, 99);
            Add_Goto (Table.States (46), 58, 100);
            Add_Goto (Table.States (46), 59, 101);
            Add_Goto (Table.States (46), 60, 102);
            Add_Goto (Table.States (46), 61, 103);
            Add_Goto (Table.States (46), 62, 104);
            Add_Goto (Table.States (46), 63, 105);
            Add_Goto (Table.States (46), 64, 106);
            Table.States (46).Kernel := To_Vector ((0 => ((62, 0),  26,  2, (32767, 0),  0)));
            Table.States (46).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 97)));
            Table.States (47).Action_List.Set_Capacity (6);
            Add_Action (Table.States (47), 25, (63, 0), 107);
            Add_Action (Table.States (47), 26, (62, 0), 108);
            Add_Action (Table.States (47), 27, (61, 0), 109);
            Add_Action (Table.States (47), 28, (57, 0), 110);
            Add_Action (Table.States (47), 39, (58, 1), 111);
            Add_Action (Table.States (47), 41, (60, 1), 112);
            Table.States (47).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (47), 57, 113);
            Add_Goto (Table.States (47), 58, 114);
            Add_Goto (Table.States (47), 59, 115);
            Add_Goto (Table.States (47), 60, 116);
            Add_Goto (Table.States (47), 61, 117);
            Add_Goto (Table.States (47), 62, 118);
            Add_Goto (Table.States (47), 63, 119);
            Add_Goto (Table.States (47), 64, 120);
            Table.States (47).Kernel := To_Vector ((((61, 0),  27,  2, (32767, 0),  0), ((62, 1),  27,  3, (32767, 0),
            0), ((63, 2),  27,  3, (32767, 0),  0), ((63, 3),  27,  3, (32767, 0),  0)));
            Table.States (47).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 111)));
            Table.States (48).Action_List.Set_Capacity (1);
            Add_Action (Table.States (48), 39, (57, 0), 121);
            Table.States (48).Kernel := To_Vector ((0 => ((57, 0),  28,  4, (32767, 0),  0)));
            Table.States (48).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  39, 121)));
            Table.States (49).Action_List.Set_Capacity (14);
            Add_Action (Table.States (49), 19, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 20, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 23, (58, 1), 122);
            Add_Action (Table.States (49), 25, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 26, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 27, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 28, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 30, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 31, (63, 4), 123);
            Add_Action (Table.States (49), 32, (62, 2), 124);
            Add_Action (Table.States (49), 36, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 37, (63, 5), 125);
            Add_Action (Table.States (49), 39, Reduce, (60, 0),  1);
            Add_Action (Table.States (49), 41, Reduce, (60, 0),  1);
            Table.States (49).Kernel := To_Vector ((((58, 1),  39,  2, (32767, 0),  0), ((60, 0),  39,  0, (60, 0),
            1), ((62, 2),  39,  1, (32767, 0),  0), ((63, 4),  39,  1, (32767, 0),  0), ((63, 5),  39,  1, (32767, 0),
            0)));
            Table.States (49).Minimal_Complete_Actions := To_Vector (((Reduce, (60, 0),  1), (Shift, (62, 2),  32,
            124), (Shift, (63, 4),  31, 123), (Shift, (63, 5),  37, 125)));
            Table.States (50).Action_List.Set_Capacity (11);
            Add_Action (Table.States (50), 19, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 20, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 25, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 26, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 27, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 28, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 30, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 32, (62, 3), 126);
            Add_Action (Table.States (50), 36, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 39, Reduce, (60, 1),  1);
            Add_Action (Table.States (50), 41, Reduce, (60, 1),  1);
            Table.States (50).Kernel := To_Vector ((((60, 1),  41,  0, (60, 1),  1), ((62, 3),  41,  1, (32767, 0),
            0)));
            Table.States (50).Minimal_Complete_Actions := To_Vector (((Reduce, (60, 1),  1), (Shift, (62, 3),  32,
            126)));
         end Subr_1;
         procedure Subr_2
         is begin
            Table.States (51).Action_List.Set_Capacity (3);
            Add_Action (Table.States (51), 20, (54, 0), 127);
            Add_Action (Table.States (51), 30, (55, 2), 128);
            Add_Action (Table.States (51), 36, (54, 1), 129);
            Table.States (51).Kernel := To_Vector ((((54, 0),  55,  2, (32767, 0),  0), ((54, 1),  55,  1, (32767, 0),
            0), ((55, 1),  55,  2, (32767, 0),  0), ((55, 2),  55,  5, (32767, 0),  0), ((55, 3),  55,  5, (32767, 0),
            0), ((55, 4),  55,  5, (32767, 0),  0), ((55, 5),  55,  5, (32767, 0),  0), ((55, 6),  55,  3, (32767, 0),
            0)));
            Table.States (51).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (54, 1),  36, 129)));
            Table.States (52).Action_List.Set_Capacity (3);
            Add_Action (Table.States (52), (20, 30, 36), (55, 0),  1);
            Table.States (52).Kernel := To_Vector ((0 => ((55, 0),  56,  0, (55, 0),  1)));
            Table.States (52).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 0),  1)));
            Table.States (53).Action_List.Set_Capacity (10);
            Add_Action (Table.States (53), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (60, 2),  1);
            Table.States (53).Kernel := To_Vector ((0 => ((60, 2),  57,  0, (60, 2),  1)));
            Table.States (53).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 2),  1)));
            Table.States (54).Action_List.Set_Capacity (10);
            Add_Action (Table.States (54), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (59, 0),  1);
            Table.States (54).Kernel := To_Vector ((0 => ((59, 0),  58,  0, (59, 0),  1)));
            Table.States (54).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (59, 0),  1)));
            Table.States (55).Action_List.Set_Capacity (10);
            Add_Action (Table.States (55), 19, (56, 1), 130);
            Add_Action (Table.States (55), 20, Reduce, (56, 0),  1);
            Add_Action (Table.States (55), 25, (63, 0), 45);
            Add_Action (Table.States (55), 26, (62, 0), 46);
            Add_Action (Table.States (55), 27, (61, 0), 47);
            Add_Action (Table.States (55), 28, (57, 0), 48);
            Add_Action (Table.States (55), 30, Reduce, (56, 0),  1);
            Add_Action (Table.States (55), 36, Reduce, (56, 0),  1);
            Add_Action (Table.States (55), 39, (58, 1), 49);
            Add_Action (Table.States (55), 41, (60, 1), 50);
            Table.States (55).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (55), 57, 53);
            Add_Goto (Table.States (55), 58, 131);
            Add_Goto (Table.States (55), 60, 56);
            Add_Goto (Table.States (55), 61, 57);
            Add_Goto (Table.States (55), 62, 58);
            Add_Goto (Table.States (55), 63, 59);
            Table.States (55).Kernel := To_Vector ((((56, 0),  59,  0, (56, 0),  1), ((56, 1),  59,  1, (32767, 0),
            0), ((56, 2),  59,  2, (32767, 0),  0), ((59, 1),  59,  1, (32767, 0),  0)));
            Table.States (55).Minimal_Complete_Actions := To_Vector (((Reduce, (56, 0),  1), (Shift, (56, 1),  19,
            130)));
            Table.States (56).Action_List.Set_Capacity (10);
            Add_Action (Table.States (56), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (58, 0),  1);
            Table.States (56).Kernel := To_Vector ((0 => ((58, 0),  60,  0, (58, 0),  1)));
            Table.States (56).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 0),  1)));
            Table.States (57).Action_List.Set_Capacity (10);
            Add_Action (Table.States (57), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (60, 5),  1);
            Table.States (57).Kernel := To_Vector ((0 => ((60, 5),  61,  0, (60, 5),  1)));
            Table.States (57).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 5),  1)));
            Table.States (58).Action_List.Set_Capacity (10);
            Add_Action (Table.States (58), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (60, 3),  1);
            Table.States (58).Kernel := To_Vector ((0 => ((60, 3),  62,  0, (60, 3),  1)));
            Table.States (58).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 3),  1)));
            Table.States (59).Action_List.Set_Capacity (10);
            Add_Action (Table.States (59), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (60, 4),  1);
            Table.States (59).Kernel := To_Vector ((0 => ((60, 4),  63,  0, (60, 4),  1)));
            Table.States (59).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 4),  1)));
            Table.States (60).Action_List.Set_Capacity (3);
            Add_Action (Table.States (60), (30, 39, 42), (48, 3),  4);
            Table.States (60).Kernel := To_Vector ((0 => ((48, 3),  17,  0, (48, 3),  4)));
            Table.States (60).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 3),  4)));
            Table.States (61).Action_List.Set_Capacity (2);
            Add_Action (Table.States (61), (17, 39), (49, 1),  2);
            Table.States (61).Kernel := To_Vector ((0 => ((49, 1),  39,  0, (49, 1),  2)));
            Table.States (61).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 1),  2)));
            Table.States (62).Action_List.Set_Capacity (2);
            Add_Action (Table.States (62), (13, 20), (45, 2),  2);
            Table.States (62).Kernel := To_Vector ((0 => ((45, 2),  39,  0, (45, 2),  2)));
            Table.States (62).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 2),  2)));
            Table.States (63).Action_List.Set_Capacity (2);
            Add_Action (Table.States (63), (13, 20), (45, 1),  2);
            Table.States (63).Kernel := To_Vector ((0 => ((45, 1),  39,  0, (45, 1),  2)));
            Table.States (63).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 1),  2)));
            Table.States (64).Action_List.Set_Capacity (2);
            Add_Action (Table.States (64), (13, 20), (45, 0),  2);
            Table.States (64).Kernel := To_Vector ((0 => ((45, 0),  39,  0, (45, 0),  2)));
            Table.States (64).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 0),  2)));
            Table.States (65).Action_List.Set_Capacity (1);
            Add_Action (Table.States (65), 16, (48, 4), 132);
            Table.States (65).Kernel := To_Vector ((0 => ((48, 4),  13,  2, (32767, 0),  0)));
            Table.States (65).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 4),  16, 132)));
            Table.States (66).Action_List.Set_Capacity (4);
            Add_Action (Table.States (66), 3, (45, 2), 24);
            Add_Action (Table.States (66), 14, (45, 1), 25);
            Add_Action (Table.States (66), 15, (45, 0), 26);
            Add_Action (Table.States (66), 39, (45, 3), 27);
            Table.States (66).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (66), 45, 133);
            Add_Goto (Table.States (66), 46, 134);
            Table.States (66).Kernel := To_Vector ((((46, 1),  20,  1, (32767, 0),  0), ((46, 2),  20,  1, (32767, 0),
            0)));
            Table.States (67).Action_List.Set_Capacity (1);
            Add_Action (Table.States (67), 16, (48, 5), 135);
            Table.States (67).Kernel := To_Vector ((0 => ((48, 5),  13,  3, (32767, 0),  0)));
            Table.States (67).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 5),  16, 135)));
            Table.States (68).Action_List.Set_Capacity (1);
            Add_Action (Table.States (68), 39, (50, 0), 136);
            Table.States (68).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (68), 50, 137);
            Table.States (68).Kernel := To_Vector ((0 => ((48, 11),  10,  1, (32767, 0),  0)));
            Table.States (68).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  39, 136)));
            Table.States (69).Action_List.Set_Capacity (1);
            Add_Action (Table.States (69), 39, (48, 10), 138);
            Table.States (69).Kernel := To_Vector ((0 => ((48, 10),  23,  1, (32767, 0),  0)));
            Table.States (69).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 10),  39, 138)));
            Table.States (70).Action_List.Set_Capacity (1);
            Add_Action (Table.States (70), 39, (50, 0), 136);
            Table.States (70).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (70), 50, 139);
            Table.States (70).Kernel := To_Vector ((0 => ((48, 9),  10,  1, (32767, 0),  0)));
            Table.States (70).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  39, 136)));
            Table.States (71).Action_List.Set_Capacity (1);
            Add_Action (Table.States (71), 39, (48, 8), 140);
            Table.States (71).Kernel := To_Vector ((0 => ((48, 8),  23,  1, (32767, 0),  0)));
            Table.States (71).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 8),  39, 140)));
            Table.States (72).Action_List.Set_Capacity (1);
            Add_Action (Table.States (72), 24, (47, 1), 141);
            Table.States (72).Kernel := To_Vector ((0 => ((47, 1),  39,  1, (32767, 0),  0)));
            Table.States (72).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 1),  24, 141)));
            Table.States (73).Action_List.Set_Capacity (1);
            Add_Action (Table.States (73), 24, (47, 2), 142);
            Table.States (73).Kernel := To_Vector ((0 => ((47, 2),  39,  1, (32767, 0),  0)));
            Table.States (73).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 2),  24, 142)));
            Table.States (74).Action_List.Set_Capacity (7);
            Add_Action (Table.States (74), (18, 30, 38, 39, 40, 41, 42), (52, 1),  2);
            Table.States (74).Kernel := To_Vector ((0 => ((52, 1),  51,  0, (52, 1),  2)));
            Table.States (74).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 1),  2)));
            Table.States (75).Action_List.Set_Capacity (6);
            Add_Action (Table.States (75), (18, 30, 39, 40, 41, 42), (44, 0),  1);
            Table.States (75).Kernel := To_Vector ((0 => ((44, 0),  18,  0, (44, 0),  1)));
            Table.States (75).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 0),  1)));
            Table.States (76).Action_List.Set_Capacity (6);
            Add_Action (Table.States (76), (18, 30, 39, 40, 41, 42), (44, 1),  1);
            Table.States (76).Kernel := To_Vector ((0 => ((44, 1),  40,  0, (44, 1),  1)));
            Table.States (76).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 1),  1)));
            Table.States (77).Action_List.Set_Capacity (6);
            Add_Action (Table.States (77), (18, 30, 39, 40, 41, 42), (44, 2),  1);
            Table.States (77).Kernel := To_Vector ((0 => ((44, 2),  41,  0, (44, 2),  1)));
            Table.States (77).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 2),  1)));
            Table.States (78).Action_List.Set_Capacity (6);
            Add_Action (Table.States (78), 18, (44, 0), 143);
            Add_Action (Table.States (78), 30, Reduce, (48, 1),  4);
            Add_Action (Table.States (78), 39, Reduce, (48, 1),  4);
            Add_Action (Table.States (78), 40, (44, 1), 144);
            Add_Action (Table.States (78), 41, (44, 2), 145);
            Add_Action (Table.States (78), 42, Reduce, (48, 1),  4);
            Table.States (78).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (78), 44, 146);
            Table.States (78).Kernel := To_Vector ((((48, 0),  44,  1, (32767, 0),  0), ((48, 1),  44,  0, (48, 1),
            4)));
            Table.States (78).Minimal_Complete_Actions := To_Vector (((Shift, (44, 0),  18, 143), (Reduce, (48, 1),
            4)));
            Table.States (79).Action_List.Set_Capacity (6);
            Add_Action (Table.States (79), 25, (63, 0), 79);
            Add_Action (Table.States (79), 26, (62, 0), 80);
            Add_Action (Table.States (79), 27, (61, 0), 81);
            Add_Action (Table.States (79), 28, (57, 0), 82);
            Add_Action (Table.States (79), 39, (58, 1), 83);
            Add_Action (Table.States (79), 41, (60, 1), 84);
            Table.States (79).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (79), 57, 85);
            Add_Goto (Table.States (79), 58, 86);
            Add_Goto (Table.States (79), 59, 87);
            Add_Goto (Table.States (79), 60, 88);
            Add_Goto (Table.States (79), 61, 89);
            Add_Goto (Table.States (79), 62, 90);
            Add_Goto (Table.States (79), 63, 91);
            Add_Goto (Table.States (79), 64, 147);
            Table.States (79).Kernel := To_Vector ((((63, 0),  25,  2, (32767, 0),  0), ((63, 1),  25,  3, (32767, 0),
            0)));
            Table.States (79).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 83)));
            Table.States (80).Action_List.Set_Capacity (6);
            Add_Action (Table.States (80), 25, (63, 0), 93);
            Add_Action (Table.States (80), 26, (62, 0), 94);
            Add_Action (Table.States (80), 27, (61, 0), 95);
            Add_Action (Table.States (80), 28, (57, 0), 96);
            Add_Action (Table.States (80), 39, (58, 1), 97);
            Add_Action (Table.States (80), 41, (60, 1), 98);
            Table.States (80).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (80), 57, 99);
            Add_Goto (Table.States (80), 58, 100);
            Add_Goto (Table.States (80), 59, 101);
            Add_Goto (Table.States (80), 60, 102);
            Add_Goto (Table.States (80), 61, 103);
            Add_Goto (Table.States (80), 62, 104);
            Add_Goto (Table.States (80), 63, 105);
            Add_Goto (Table.States (80), 64, 148);
            Table.States (80).Kernel := To_Vector ((0 => ((62, 0),  26,  2, (32767, 0),  0)));
            Table.States (80).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 97)));
            Table.States (81).Action_List.Set_Capacity (6);
            Add_Action (Table.States (81), 25, (63, 0), 107);
            Add_Action (Table.States (81), 26, (62, 0), 108);
            Add_Action (Table.States (81), 27, (61, 0), 109);
            Add_Action (Table.States (81), 28, (57, 0), 110);
            Add_Action (Table.States (81), 39, (58, 1), 111);
            Add_Action (Table.States (81), 41, (60, 1), 112);
            Table.States (81).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (81), 57, 113);
            Add_Goto (Table.States (81), 58, 114);
            Add_Goto (Table.States (81), 59, 115);
            Add_Goto (Table.States (81), 60, 116);
            Add_Goto (Table.States (81), 61, 117);
            Add_Goto (Table.States (81), 62, 118);
            Add_Goto (Table.States (81), 63, 119);
            Add_Goto (Table.States (81), 64, 149);
            Table.States (81).Kernel := To_Vector ((((61, 0),  27,  2, (32767, 0),  0), ((62, 1),  27,  3, (32767, 0),
            0), ((63, 2),  27,  3, (32767, 0),  0), ((63, 3),  27,  3, (32767, 0),  0)));
            Table.States (81).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 111)));
            Table.States (82).Action_List.Set_Capacity (1);
            Add_Action (Table.States (82), 39, (57, 0), 150);
            Table.States (82).Kernel := To_Vector ((0 => ((57, 0),  28,  4, (32767, 0),  0)));
            Table.States (82).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  39, 150)));
            Table.States (83).Action_List.Set_Capacity (12);
            Add_Action (Table.States (83), 20, Reduce, (60, 0),  1);
            Add_Action (Table.States (83), 23, (58, 1), 151);
            Add_Action (Table.States (83), 25, Reduce, (60, 0),  1);
            Add_Action (Table.States (83), 26, Reduce, (60, 0),  1);
            Add_Action (Table.States (83), 27, Reduce, (60, 0),  1);
            Add_Action (Table.States (83), 28, Reduce, (60, 0),  1);
            Add_Action (Table.States (83), 31, (63, 4), 152);
            Add_Action (Table.States (83), 32, (62, 2), 153);
            Add_Action (Table.States (83), 33, Reduce, (60, 0),  1);
            Add_Action (Table.States (83), 37, (63, 5), 154);
            Add_Action (Table.States (83), 39, Reduce, (60, 0),  1);
            Add_Action (Table.States (83), 41, Reduce, (60, 0),  1);
            Table.States (83).Kernel := To_Vector ((((58, 1),  39,  2, (32767, 0),  0), ((60, 0),  39,  0, (60, 0),
            1), ((62, 2),  39,  1, (32767, 0),  0), ((63, 4),  39,  1, (32767, 0),  0), ((63, 5),  39,  1, (32767, 0),
            0)));
            Table.States (83).Minimal_Complete_Actions := To_Vector (((Reduce, (60, 0),  1), (Shift, (62, 2),  32,
            153), (Shift, (63, 4),  31, 152), (Shift, (63, 5),  37, 154)));
            Table.States (84).Action_List.Set_Capacity (9);
            Add_Action (Table.States (84), 20, Reduce, (60, 1),  1);
            Add_Action (Table.States (84), 25, Reduce, (60, 1),  1);
            Add_Action (Table.States (84), 26, Reduce, (60, 1),  1);
            Add_Action (Table.States (84), 27, Reduce, (60, 1),  1);
            Add_Action (Table.States (84), 28, Reduce, (60, 1),  1);
            Add_Action (Table.States (84), 32, (62, 3), 155);
            Add_Action (Table.States (84), 33, Reduce, (60, 1),  1);
            Add_Action (Table.States (84), 39, Reduce, (60, 1),  1);
            Add_Action (Table.States (84), 41, Reduce, (60, 1),  1);
            Table.States (84).Kernel := To_Vector ((((60, 1),  41,  0, (60, 1),  1), ((62, 3),  41,  1, (32767, 0),
            0)));
            Table.States (84).Minimal_Complete_Actions := To_Vector (((Reduce, (60, 1),  1), (Shift, (62, 3),  32,
            155)));
            Table.States (85).Action_List.Set_Capacity (8);
            Add_Action (Table.States (85), (20, 25, 26, 27, 28, 33, 39, 41), (60, 2),  1);
            Table.States (85).Kernel := To_Vector ((0 => ((60, 2),  57,  0, (60, 2),  1)));
            Table.States (85).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 2),  1)));
            Table.States (86).Action_List.Set_Capacity (8);
            Add_Action (Table.States (86), (20, 25, 26, 27, 28, 33, 39, 41), (59, 0),  1);
            Table.States (86).Kernel := To_Vector ((0 => ((59, 0),  58,  0, (59, 0),  1)));
            Table.States (86).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (59, 0),  1)));
            Table.States (87).Action_List.Set_Capacity (8);
            Add_Action (Table.States (87), 20, Reduce, (64, 0),  1);
            Add_Action (Table.States (87), 25, (63, 0), 79);
            Add_Action (Table.States (87), 26, (62, 0), 80);
            Add_Action (Table.States (87), 27, (61, 0), 81);
            Add_Action (Table.States (87), 28, (57, 0), 82);
            Add_Action (Table.States (87), 33, Reduce, (64, 0),  1);
            Add_Action (Table.States (87), 39, (58, 1), 83);
            Add_Action (Table.States (87), 41, (60, 1), 84);
            Table.States (87).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (87), 57, 85);
            Add_Goto (Table.States (87), 58, 156);
            Add_Goto (Table.States (87), 60, 88);
            Add_Goto (Table.States (87), 61, 89);
            Add_Goto (Table.States (87), 62, 90);
            Add_Goto (Table.States (87), 63, 91);
            Table.States (87).Kernel := To_Vector ((((59, 1),  59,  1, (32767, 0),  0), ((64, 0),  59,  0, (64, 0),
            1)));
            Table.States (87).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (64, 0),  1)));
            Table.States (88).Action_List.Set_Capacity (8);
            Add_Action (Table.States (88), (20, 25, 26, 27, 28, 33, 39, 41), (58, 0),  1);
            Table.States (88).Kernel := To_Vector ((0 => ((58, 0),  60,  0, (58, 0),  1)));
            Table.States (88).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 0),  1)));
            Table.States (89).Action_List.Set_Capacity (8);
            Add_Action (Table.States (89), (20, 25, 26, 27, 28, 33, 39, 41), (60, 5),  1);
            Table.States (89).Kernel := To_Vector ((0 => ((60, 5),  61,  0, (60, 5),  1)));
            Table.States (89).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 5),  1)));
            Table.States (90).Action_List.Set_Capacity (8);
            Add_Action (Table.States (90), (20, 25, 26, 27, 28, 33, 39, 41), (60, 3),  1);
            Table.States (90).Kernel := To_Vector ((0 => ((60, 3),  62,  0, (60, 3),  1)));
            Table.States (90).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 3),  1)));
            Table.States (91).Action_List.Set_Capacity (8);
            Add_Action (Table.States (91), (20, 25, 26, 27, 28, 33, 39, 41), (60, 4),  1);
            Table.States (91).Kernel := To_Vector ((0 => ((60, 4),  63,  0, (60, 4),  1)));
            Table.States (91).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 4),  1)));
            Table.States (92).Action_List.Set_Capacity (2);
            Add_Action (Table.States (92), 20, (64, 1), 157);
            Add_Action (Table.States (92), 33, (63, 0), 158);
            Table.States (92).Kernel := To_Vector ((((63, 0),  64,  1, (32767, 0),  0), ((63, 1),  64,  2, (32767, 0),
            0), ((64, 1),  64,  2, (32767, 0),  0)));
            Table.States (92).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (63, 0),  33, 158)));
            Table.States (93).Action_List.Set_Capacity (6);
            Add_Action (Table.States (93), 25, (63, 0), 79);
            Add_Action (Table.States (93), 26, (62, 0), 80);
            Add_Action (Table.States (93), 27, (61, 0), 81);
            Add_Action (Table.States (93), 28, (57, 0), 82);
            Add_Action (Table.States (93), 39, (58, 1), 83);
            Add_Action (Table.States (93), 41, (60, 1), 84);
            Table.States (93).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (93), 57, 85);
            Add_Goto (Table.States (93), 58, 86);
            Add_Goto (Table.States (93), 59, 87);
            Add_Goto (Table.States (93), 60, 88);
            Add_Goto (Table.States (93), 61, 89);
            Add_Goto (Table.States (93), 62, 90);
            Add_Goto (Table.States (93), 63, 91);
            Add_Goto (Table.States (93), 64, 159);
            Table.States (93).Kernel := To_Vector ((((63, 0),  25,  2, (32767, 0),  0), ((63, 1),  25,  3, (32767, 0),
            0)));
            Table.States (93).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 83)));
            Table.States (94).Action_List.Set_Capacity (6);
            Add_Action (Table.States (94), 25, (63, 0), 93);
            Add_Action (Table.States (94), 26, (62, 0), 94);
            Add_Action (Table.States (94), 27, (61, 0), 95);
            Add_Action (Table.States (94), 28, (57, 0), 96);
            Add_Action (Table.States (94), 39, (58, 1), 97);
            Add_Action (Table.States (94), 41, (60, 1), 98);
            Table.States (94).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (94), 57, 99);
            Add_Goto (Table.States (94), 58, 100);
            Add_Goto (Table.States (94), 59, 101);
            Add_Goto (Table.States (94), 60, 102);
            Add_Goto (Table.States (94), 61, 103);
            Add_Goto (Table.States (94), 62, 104);
            Add_Goto (Table.States (94), 63, 105);
            Add_Goto (Table.States (94), 64, 160);
            Table.States (94).Kernel := To_Vector ((0 => ((62, 0),  26,  2, (32767, 0),  0)));
            Table.States (94).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 97)));
            Table.States (95).Action_List.Set_Capacity (6);
            Add_Action (Table.States (95), 25, (63, 0), 107);
            Add_Action (Table.States (95), 26, (62, 0), 108);
            Add_Action (Table.States (95), 27, (61, 0), 109);
            Add_Action (Table.States (95), 28, (57, 0), 110);
            Add_Action (Table.States (95), 39, (58, 1), 111);
            Add_Action (Table.States (95), 41, (60, 1), 112);
            Table.States (95).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (95), 57, 113);
            Add_Goto (Table.States (95), 58, 114);
            Add_Goto (Table.States (95), 59, 115);
            Add_Goto (Table.States (95), 60, 116);
            Add_Goto (Table.States (95), 61, 117);
            Add_Goto (Table.States (95), 62, 118);
            Add_Goto (Table.States (95), 63, 119);
            Add_Goto (Table.States (95), 64, 161);
            Table.States (95).Kernel := To_Vector ((((61, 0),  27,  2, (32767, 0),  0), ((62, 1),  27,  3, (32767, 0),
            0), ((63, 2),  27,  3, (32767, 0),  0), ((63, 3),  27,  3, (32767, 0),  0)));
            Table.States (95).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 111)));
            Table.States (96).Action_List.Set_Capacity (1);
            Add_Action (Table.States (96), 39, (57, 0), 162);
            Table.States (96).Kernel := To_Vector ((0 => ((57, 0),  28,  4, (32767, 0),  0)));
            Table.States (96).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  39, 162)));
            Table.States (97).Action_List.Set_Capacity (12);
            Add_Action (Table.States (97), 20, Reduce, (60, 0),  1);
            Add_Action (Table.States (97), 23, (58, 1), 163);
            Add_Action (Table.States (97), 25, Reduce, (60, 0),  1);
            Add_Action (Table.States (97), 26, Reduce, (60, 0),  1);
            Add_Action (Table.States (97), 27, Reduce, (60, 0),  1);
            Add_Action (Table.States (97), 28, Reduce, (60, 0),  1);
            Add_Action (Table.States (97), 31, (63, 4), 164);
            Add_Action (Table.States (97), 32, (62, 2), 165);
            Add_Action (Table.States (97), 34, Reduce, (60, 0),  1);
            Add_Action (Table.States (97), 37, (63, 5), 166);
            Add_Action (Table.States (97), 39, Reduce, (60, 0),  1);
            Add_Action (Table.States (97), 41, Reduce, (60, 0),  1);
            Table.States (97).Kernel := To_Vector ((((58, 1),  39,  2, (32767, 0),  0), ((60, 0),  39,  0, (60, 0),
            1), ((62, 2),  39,  1, (32767, 0),  0), ((63, 4),  39,  1, (32767, 0),  0), ((63, 5),  39,  1, (32767, 0),
            0)));
            Table.States (97).Minimal_Complete_Actions := To_Vector (((Reduce, (60, 0),  1), (Shift, (62, 2),  32,
            165), (Shift, (63, 4),  31, 164), (Shift, (63, 5),  37, 166)));
         end Subr_2;
         procedure Subr_3
         is begin
            Table.States (98).Action_List.Set_Capacity (9);
            Add_Action (Table.States (98), 20, Reduce, (60, 1),  1);
            Add_Action (Table.States (98), 25, Reduce, (60, 1),  1);
            Add_Action (Table.States (98), 26, Reduce, (60, 1),  1);
            Add_Action (Table.States (98), 27, Reduce, (60, 1),  1);
            Add_Action (Table.States (98), 28, Reduce, (60, 1),  1);
            Add_Action (Table.States (98), 32, (62, 3), 167);
            Add_Action (Table.States (98), 34, Reduce, (60, 1),  1);
            Add_Action (Table.States (98), 39, Reduce, (60, 1),  1);
            Add_Action (Table.States (98), 41, Reduce, (60, 1),  1);
            Table.States (98).Kernel := To_Vector ((((60, 1),  41,  0, (60, 1),  1), ((62, 3),  41,  1, (32767, 0),
            0)));
            Table.States (98).Minimal_Complete_Actions := To_Vector (((Reduce, (60, 1),  1), (Shift, (62, 3),  32,
            167)));
            Table.States (99).Action_List.Set_Capacity (8);
            Add_Action (Table.States (99), (20, 25, 26, 27, 28, 34, 39, 41), (60, 2),  1);
            Table.States (99).Kernel := To_Vector ((0 => ((60, 2),  57,  0, (60, 2),  1)));
            Table.States (99).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 2),  1)));
            Table.States (100).Action_List.Set_Capacity (8);
            Add_Action (Table.States (100), (20, 25, 26, 27, 28, 34, 39, 41), (59, 0),  1);
            Table.States (100).Kernel := To_Vector ((0 => ((59, 0),  58,  0, (59, 0),  1)));
            Table.States (100).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (59, 0),  1)));
            Table.States (101).Action_List.Set_Capacity (8);
            Add_Action (Table.States (101), 20, Reduce, (64, 0),  1);
            Add_Action (Table.States (101), 25, (63, 0), 93);
            Add_Action (Table.States (101), 26, (62, 0), 94);
            Add_Action (Table.States (101), 27, (61, 0), 95);
            Add_Action (Table.States (101), 28, (57, 0), 96);
            Add_Action (Table.States (101), 34, Reduce, (64, 0),  1);
            Add_Action (Table.States (101), 39, (58, 1), 97);
            Add_Action (Table.States (101), 41, (60, 1), 98);
            Table.States (101).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (101), 57, 99);
            Add_Goto (Table.States (101), 58, 168);
            Add_Goto (Table.States (101), 60, 102);
            Add_Goto (Table.States (101), 61, 103);
            Add_Goto (Table.States (101), 62, 104);
            Add_Goto (Table.States (101), 63, 105);
            Table.States (101).Kernel := To_Vector ((((59, 1),  59,  1, (32767, 0),  0), ((64, 0),  59,  0, (64, 0),
            1)));
            Table.States (101).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (64, 0),  1)));
            Table.States (102).Action_List.Set_Capacity (8);
            Add_Action (Table.States (102), (20, 25, 26, 27, 28, 34, 39, 41), (58, 0),  1);
            Table.States (102).Kernel := To_Vector ((0 => ((58, 0),  60,  0, (58, 0),  1)));
            Table.States (102).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 0),  1)));
            Table.States (103).Action_List.Set_Capacity (8);
            Add_Action (Table.States (103), (20, 25, 26, 27, 28, 34, 39, 41), (60, 5),  1);
            Table.States (103).Kernel := To_Vector ((0 => ((60, 5),  61,  0, (60, 5),  1)));
            Table.States (103).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 5),  1)));
            Table.States (104).Action_List.Set_Capacity (8);
            Add_Action (Table.States (104), (20, 25, 26, 27, 28, 34, 39, 41), (60, 3),  1);
            Table.States (104).Kernel := To_Vector ((0 => ((60, 3),  62,  0, (60, 3),  1)));
            Table.States (104).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 3),  1)));
            Table.States (105).Action_List.Set_Capacity (8);
            Add_Action (Table.States (105), (20, 25, 26, 27, 28, 34, 39, 41), (60, 4),  1);
            Table.States (105).Kernel := To_Vector ((0 => ((60, 4),  63,  0, (60, 4),  1)));
            Table.States (105).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 4),  1)));
            Table.States (106).Action_List.Set_Capacity (2);
            Add_Action (Table.States (106), 20, (64, 1), 169);
            Add_Action (Table.States (106), 34, (62, 0), 170);
            Table.States (106).Kernel := To_Vector ((((62, 0),  64,  1, (32767, 0),  0), ((64, 1),  64,  2, (32767, 0),
             0)));
            Table.States (106).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (62, 0),  34, 170)));
            Table.States (107).Action_List.Set_Capacity (6);
            Add_Action (Table.States (107), 25, (63, 0), 79);
            Add_Action (Table.States (107), 26, (62, 0), 80);
            Add_Action (Table.States (107), 27, (61, 0), 81);
            Add_Action (Table.States (107), 28, (57, 0), 82);
            Add_Action (Table.States (107), 39, (58, 1), 83);
            Add_Action (Table.States (107), 41, (60, 1), 84);
            Table.States (107).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (107), 57, 85);
            Add_Goto (Table.States (107), 58, 86);
            Add_Goto (Table.States (107), 59, 87);
            Add_Goto (Table.States (107), 60, 88);
            Add_Goto (Table.States (107), 61, 89);
            Add_Goto (Table.States (107), 62, 90);
            Add_Goto (Table.States (107), 63, 91);
            Add_Goto (Table.States (107), 64, 171);
            Table.States (107).Kernel := To_Vector ((((63, 0),  25,  2, (32767, 0),  0), ((63, 1),  25,  3, (32767, 0),
             0)));
            Table.States (107).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 83)));
            Table.States (108).Action_List.Set_Capacity (6);
            Add_Action (Table.States (108), 25, (63, 0), 93);
            Add_Action (Table.States (108), 26, (62, 0), 94);
            Add_Action (Table.States (108), 27, (61, 0), 95);
            Add_Action (Table.States (108), 28, (57, 0), 96);
            Add_Action (Table.States (108), 39, (58, 1), 97);
            Add_Action (Table.States (108), 41, (60, 1), 98);
            Table.States (108).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (108), 57, 99);
            Add_Goto (Table.States (108), 58, 100);
            Add_Goto (Table.States (108), 59, 101);
            Add_Goto (Table.States (108), 60, 102);
            Add_Goto (Table.States (108), 61, 103);
            Add_Goto (Table.States (108), 62, 104);
            Add_Goto (Table.States (108), 63, 105);
            Add_Goto (Table.States (108), 64, 172);
            Table.States (108).Kernel := To_Vector ((0 => ((62, 0),  26,  2, (32767, 0),  0)));
            Table.States (108).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 97)));
            Table.States (109).Action_List.Set_Capacity (6);
            Add_Action (Table.States (109), 25, (63, 0), 107);
            Add_Action (Table.States (109), 26, (62, 0), 108);
            Add_Action (Table.States (109), 27, (61, 0), 109);
            Add_Action (Table.States (109), 28, (57, 0), 110);
            Add_Action (Table.States (109), 39, (58, 1), 111);
            Add_Action (Table.States (109), 41, (60, 1), 112);
            Table.States (109).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (109), 57, 113);
            Add_Goto (Table.States (109), 58, 114);
            Add_Goto (Table.States (109), 59, 115);
            Add_Goto (Table.States (109), 60, 116);
            Add_Goto (Table.States (109), 61, 117);
            Add_Goto (Table.States (109), 62, 118);
            Add_Goto (Table.States (109), 63, 119);
            Add_Goto (Table.States (109), 64, 173);
            Table.States (109).Kernel := To_Vector ((((61, 0),  27,  2, (32767, 0),  0), ((62, 1),  27,  3, (32767, 0),
             0), ((63, 2),  27,  3, (32767, 0),  0), ((63, 3),  27,  3, (32767, 0),  0)));
            Table.States (109).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 111)));
            Table.States (110).Action_List.Set_Capacity (1);
            Add_Action (Table.States (110), 39, (57, 0), 174);
            Table.States (110).Kernel := To_Vector ((0 => ((57, 0),  28,  4, (32767, 0),  0)));
            Table.States (110).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  39, 174)));
            Table.States (111).Action_List.Set_Capacity (12);
            Add_Action (Table.States (111), 20, Reduce, (60, 0),  1);
            Add_Action (Table.States (111), 23, (58, 1), 175);
            Add_Action (Table.States (111), 25, Reduce, (60, 0),  1);
            Add_Action (Table.States (111), 26, Reduce, (60, 0),  1);
            Add_Action (Table.States (111), 27, Reduce, (60, 0),  1);
            Add_Action (Table.States (111), 28, Reduce, (60, 0),  1);
            Add_Action (Table.States (111), 31, (63, 4), 176);
            Add_Action (Table.States (111), 32, (62, 2), 177);
            Add_Action (Table.States (111), 35, Reduce, (60, 0),  1);
            Add_Action (Table.States (111), 37, (63, 5), 178);
            Add_Action (Table.States (111), 39, Reduce, (60, 0),  1);
            Add_Action (Table.States (111), 41, Reduce, (60, 0),  1);
            Table.States (111).Kernel := To_Vector ((((58, 1),  39,  2, (32767, 0),  0), ((60, 0),  39,  0, (60, 0),
            1), ((62, 2),  39,  1, (32767, 0),  0), ((63, 4),  39,  1, (32767, 0),  0), ((63, 5),  39,  1, (32767, 0),
            0)));
            Table.States (111).Minimal_Complete_Actions := To_Vector (((Reduce, (60, 0),  1), (Shift, (62, 2),  32,
            177), (Shift, (63, 4),  31, 176), (Shift, (63, 5),  37, 178)));
            Table.States (112).Action_List.Set_Capacity (9);
            Add_Action (Table.States (112), 20, Reduce, (60, 1),  1);
            Add_Action (Table.States (112), 25, Reduce, (60, 1),  1);
            Add_Action (Table.States (112), 26, Reduce, (60, 1),  1);
            Add_Action (Table.States (112), 27, Reduce, (60, 1),  1);
            Add_Action (Table.States (112), 28, Reduce, (60, 1),  1);
            Add_Action (Table.States (112), 32, (62, 3), 179);
            Add_Action (Table.States (112), 35, Reduce, (60, 1),  1);
            Add_Action (Table.States (112), 39, Reduce, (60, 1),  1);
            Add_Action (Table.States (112), 41, Reduce, (60, 1),  1);
            Table.States (112).Kernel := To_Vector ((((60, 1),  41,  0, (60, 1),  1), ((62, 3),  41,  1, (32767, 0),
            0)));
            Table.States (112).Minimal_Complete_Actions := To_Vector (((Reduce, (60, 1),  1), (Shift, (62, 3),  32,
            179)));
            Table.States (113).Action_List.Set_Capacity (8);
            Add_Action (Table.States (113), (20, 25, 26, 27, 28, 35, 39, 41), (60, 2),  1);
            Table.States (113).Kernel := To_Vector ((0 => ((60, 2),  57,  0, (60, 2),  1)));
            Table.States (113).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 2),  1)));
            Table.States (114).Action_List.Set_Capacity (8);
            Add_Action (Table.States (114), (20, 25, 26, 27, 28, 35, 39, 41), (59, 0),  1);
            Table.States (114).Kernel := To_Vector ((0 => ((59, 0),  58,  0, (59, 0),  1)));
            Table.States (114).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (59, 0),  1)));
            Table.States (115).Action_List.Set_Capacity (8);
            Add_Action (Table.States (115), 20, Reduce, (64, 0),  1);
            Add_Action (Table.States (115), 25, (63, 0), 107);
            Add_Action (Table.States (115), 26, (62, 0), 108);
            Add_Action (Table.States (115), 27, (61, 0), 109);
            Add_Action (Table.States (115), 28, (57, 0), 110);
            Add_Action (Table.States (115), 35, Reduce, (64, 0),  1);
            Add_Action (Table.States (115), 39, (58, 1), 111);
            Add_Action (Table.States (115), 41, (60, 1), 112);
            Table.States (115).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (115), 57, 113);
            Add_Goto (Table.States (115), 58, 180);
            Add_Goto (Table.States (115), 60, 116);
            Add_Goto (Table.States (115), 61, 117);
            Add_Goto (Table.States (115), 62, 118);
            Add_Goto (Table.States (115), 63, 119);
            Table.States (115).Kernel := To_Vector ((((59, 1),  59,  1, (32767, 0),  0), ((64, 0),  59,  0, (64, 0),
            1)));
            Table.States (115).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (64, 0),  1)));
            Table.States (116).Action_List.Set_Capacity (8);
            Add_Action (Table.States (116), (20, 25, 26, 27, 28, 35, 39, 41), (58, 0),  1);
            Table.States (116).Kernel := To_Vector ((0 => ((58, 0),  60,  0, (58, 0),  1)));
            Table.States (116).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 0),  1)));
            Table.States (117).Action_List.Set_Capacity (8);
            Add_Action (Table.States (117), (20, 25, 26, 27, 28, 35, 39, 41), (60, 5),  1);
            Table.States (117).Kernel := To_Vector ((0 => ((60, 5),  61,  0, (60, 5),  1)));
            Table.States (117).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 5),  1)));
            Table.States (118).Action_List.Set_Capacity (8);
            Add_Action (Table.States (118), (20, 25, 26, 27, 28, 35, 39, 41), (60, 3),  1);
            Table.States (118).Kernel := To_Vector ((0 => ((60, 3),  62,  0, (60, 3),  1)));
            Table.States (118).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 3),  1)));
            Table.States (119).Action_List.Set_Capacity (8);
            Add_Action (Table.States (119), (20, 25, 26, 27, 28, 35, 39, 41), (60, 4),  1);
            Table.States (119).Kernel := To_Vector ((0 => ((60, 4),  63,  0, (60, 4),  1)));
            Table.States (119).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (60, 4),  1)));
            Table.States (120).Action_List.Set_Capacity (2);
            Add_Action (Table.States (120), 20, (64, 1), 181);
            Add_Action (Table.States (120), 35, (61, 0), 182);
            Table.States (120).Kernel := To_Vector ((((61, 0),  64,  1, (32767, 0),  0), ((62, 1),  64,  2, (32767, 0),
             0), ((63, 2),  64,  2, (32767, 0),  0), ((63, 3),  64,  2, (32767, 0),  0), ((64, 1),  64,  2, (32767, 0),
             0)));
            Table.States (120).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (61, 0),  35, 182)));
            Table.States (121).Action_List.Set_Capacity (1);
            Add_Action (Table.States (121), 23, (57, 0), 183);
            Table.States (121).Kernel := To_Vector ((0 => ((57, 0),  39,  3, (32767, 0),  0)));
            Table.States (121).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  23, 183)));
            Table.States (122).Action_List.Set_Capacity (6);
            Add_Action (Table.States (122), 25, (63, 0), 45);
            Add_Action (Table.States (122), 26, (62, 0), 46);
            Add_Action (Table.States (122), 27, (61, 0), 47);
            Add_Action (Table.States (122), 28, (57, 0), 48);
            Add_Action (Table.States (122), 39, (60, 0), 184);
            Add_Action (Table.States (122), 41, (60, 1), 50);
            Table.States (122).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (122), 57, 53);
            Add_Goto (Table.States (122), 60, 185);
            Add_Goto (Table.States (122), 61, 57);
            Add_Goto (Table.States (122), 62, 58);
            Add_Goto (Table.States (122), 63, 59);
            Table.States (122).Kernel := To_Vector ((0 => ((58, 1),  23,  1, (32767, 0),  0)));
            Table.States (122).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (60, 0),  39, 184)));
            Table.States (123).Action_List.Set_Capacity (10);
            Add_Action (Table.States (123), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (63, 4),  2);
            Table.States (123).Kernel := To_Vector ((0 => ((63, 4),  31,  0, (63, 4),  2)));
            Table.States (123).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 4),  2)));
            Table.States (124).Action_List.Set_Capacity (10);
            Add_Action (Table.States (124), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (62, 2),  2);
            Table.States (124).Kernel := To_Vector ((0 => ((62, 2),  32,  0, (62, 2),  2)));
            Table.States (124).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 2),  2)));
            Table.States (125).Action_List.Set_Capacity (10);
            Add_Action (Table.States (125), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (63, 5),  2);
            Table.States (125).Kernel := To_Vector ((0 => ((63, 5),  37,  0, (63, 5),  2)));
            Table.States (125).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 5),  2)));
            Table.States (126).Action_List.Set_Capacity (10);
            Add_Action (Table.States (126), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (62, 3),  2);
            Table.States (126).Kernel := To_Vector ((0 => ((62, 3),  32,  0, (62, 3),  2)));
            Table.States (126).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 3),  2)));
            Table.States (127).Action_List.Set_Capacity (7);
            Add_Action (Table.States (127), 25, (63, 0), 45);
            Add_Action (Table.States (127), 26, (62, 0), 46);
            Add_Action (Table.States (127), 27, (61, 0), 47);
            Add_Action (Table.States (127), 28, (57, 0), 48);
            Add_Action (Table.States (127), 36, (54, 0), 186);
            Add_Action (Table.States (127), 39, (58, 1), 49);
            Add_Action (Table.States (127), 41, (60, 1), 50);
            Table.States (127).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (127), 56, 187);
            Add_Goto (Table.States (127), 57, 53);
            Add_Goto (Table.States (127), 58, 54);
            Add_Goto (Table.States (127), 59, 55);
            Add_Goto (Table.States (127), 60, 56);
            Add_Goto (Table.States (127), 61, 57);
            Add_Goto (Table.States (127), 62, 58);
            Add_Goto (Table.States (127), 63, 59);
            Table.States (127).Kernel := To_Vector ((((54, 0),  20,  1, (32767, 0),  0), ((55, 1),  20,  1, (32767, 0),
             0)));
            Table.States (127).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (54, 0),  36, 186)));
            Table.States (128).Action_List.Set_Capacity (3);
            Add_Action (Table.States (128), 7, (55, 6), 188);
            Add_Action (Table.States (128), 8, (55, 4), 189);
            Add_Action (Table.States (128), 9, (55, 2), 190);
            Table.States (128).Kernel := To_Vector ((((55, 2),  30,  4, (32767, 0),  0), ((55, 3),  30,  4, (32767, 0),
             0), ((55, 4),  30,  4, (32767, 0),  0), ((55, 5),  30,  4, (32767, 0),  0), ((55, 6),  30,  2, (32767, 0),
             0)));
            Table.States (129).Action_List.Set_Capacity (3);
            Add_Action (Table.States (129), (30, 39, 42), (54, 1),  4);
            Table.States (129).Kernel := To_Vector ((0 => ((54, 1),  36,  0, (54, 1),  4)));
            Table.States (129).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 1),  4)));
            Table.States (130).Action_List.Set_Capacity (4);
            Add_Action (Table.States (130), 19, (56, 2), 191);
            Add_Action (Table.States (130), 20, Reduce, (56, 1),  2);
            Add_Action (Table.States (130), 30, Reduce, (56, 1),  2);
            Add_Action (Table.States (130), 36, Reduce, (56, 1),  2);
            Table.States (130).Kernel := To_Vector ((((56, 1),  19,  0, (56, 1),  2), ((56, 2),  19,  1, (32767, 0),
            0)));
            Table.States (130).Minimal_Complete_Actions := To_Vector (((Reduce, (56, 1),  2), (Shift, (56, 2),  19,
            191)));
            Table.States (131).Action_List.Set_Capacity (10);
            Add_Action (Table.States (131), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (59, 1),  2);
            Table.States (131).Kernel := To_Vector ((0 => ((59, 1),  58,  0, (59, 1),  2)));
            Table.States (131).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (59, 1),  2)));
            Table.States (132).Action_List.Set_Capacity (1);
            Add_Action (Table.States (132), 39, (48, 4), 192);
            Table.States (132).Kernel := To_Vector ((0 => ((48, 4),  16,  1, (32767, 0),  0)));
            Table.States (132).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 4),  39, 192)));
            Table.States (133).Action_List.Set_Capacity (2);
            Add_Action (Table.States (133), (13, 20), (46, 1),  3);
            Table.States (133).Kernel := To_Vector ((((46, 0),  45,  0, (46, 0),  1), ((46, 1),  45,  0, (46, 1),
            3)));
            Table.States (133).Minimal_Complete_Actions := To_Vector (((Reduce, (46, 0),  1), (Reduce, (46, 1),  3)));
            Table.States (134).Action_List.Set_Capacity (2);
            Add_Action (Table.States (134), (13, 20), (46, 2),  3);
            Table.States (134).Kernel := To_Vector ((((46, 1),  46,  2, (32767, 0),  0), ((46, 2),  46,  0, (46, 2),
            3), ((46, 2),  46,  2, (32767, 0),  0)));
            Table.States (134).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 2),  3)));
            Table.States (135).Action_List.Set_Capacity (1);
            Add_Action (Table.States (135), 21, (48, 5), 193);
            Table.States (135).Kernel := To_Vector ((0 => ((48, 5),  16,  2, (32767, 0),  0)));
            Table.States (135).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 5),  21, 193)));
            Table.States (136).Action_List.Set_Capacity (4);
            Add_Action (Table.States (136), (20, 30, 39, 42), (50, 0),  1);
            Table.States (136).Kernel := To_Vector ((0 => ((50, 0),  39,  0, (50, 0),  1)));
            Table.States (136).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 0),  1)));
            Table.States (137).Action_List.Set_Capacity (4);
            Add_Action (Table.States (137), 20, (50, 1), 194);
            Add_Action (Table.States (137), 30, Reduce, (48, 11),  5);
            Add_Action (Table.States (137), 39, Reduce, (48, 11),  5);
            Add_Action (Table.States (137), 42, Reduce, (48, 11),  5);
            Table.States (137).Kernel := To_Vector ((((48, 11),  50,  0, (48, 11),  5), ((50, 1),  50,  2, (32767, 0),
            0)));
            Table.States (137).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 11),  5)));
            Table.States (138).Action_List.Set_Capacity (3);
            Add_Action (Table.States (138), (30, 39, 42), (48, 10),  5);
            Table.States (138).Kernel := To_Vector ((0 => ((48, 10),  39,  0, (48, 10),  5)));
            Table.States (138).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 10),  5)));
            Table.States (139).Action_List.Set_Capacity (4);
            Add_Action (Table.States (139), 20, (50, 1), 194);
            Add_Action (Table.States (139), 30, Reduce, (48, 9),  5);
            Add_Action (Table.States (139), 39, Reduce, (48, 9),  5);
            Add_Action (Table.States (139), 42, Reduce, (48, 9),  5);
            Table.States (139).Kernel := To_Vector ((((48, 9),  50,  0, (48, 9),  5), ((50, 1),  50,  2, (32767, 0),
            0)));
            Table.States (139).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 9),  5)));
            Table.States (140).Action_List.Set_Capacity (3);
            Add_Action (Table.States (140), (30, 39, 42), (48, 8),  5);
            Table.States (140).Kernel := To_Vector ((0 => ((48, 8),  39,  0, (48, 8),  5)));
            Table.States (140).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 8),  5)));
            Table.States (141).Action_List.Set_Capacity (1);
            Add_Action (Table.States (141), (1 =>  39), (47, 1),  4);
            Table.States (141).Kernel := To_Vector ((0 => ((47, 1),  24,  0, (47, 1),  4)));
            Table.States (141).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (47, 1),  4)));
            Table.States (142).Action_List.Set_Capacity (1);
            Add_Action (Table.States (142), (1 =>  39), (47, 2),  4);
            Table.States (142).Kernel := To_Vector ((0 => ((47, 2),  24,  0, (47, 2),  4)));
            Table.States (142).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (47, 2),  4)));
            Table.States (143).Action_List.Set_Capacity (3);
            Add_Action (Table.States (143), (30, 39, 42), (44, 0),  1);
            Table.States (143).Kernel := To_Vector ((0 => ((44, 0),  18,  0, (44, 0),  1)));
            Table.States (143).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 0),  1)));
            Table.States (144).Action_List.Set_Capacity (3);
            Add_Action (Table.States (144), (30, 39, 42), (44, 1),  1);
            Table.States (144).Kernel := To_Vector ((0 => ((44, 1),  40,  0, (44, 1),  1)));
            Table.States (144).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 1),  1)));
            Table.States (145).Action_List.Set_Capacity (3);
            Add_Action (Table.States (145), (30, 39, 42), (44, 2),  1);
            Table.States (145).Kernel := To_Vector ((0 => ((44, 2),  41,  0, (44, 2),  1)));
            Table.States (145).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 2),  1)));
            Table.States (146).Action_List.Set_Capacity (3);
            Add_Action (Table.States (146), (30, 39, 42), (48, 0),  5);
            Table.States (146).Kernel := To_Vector ((0 => ((48, 0),  44,  0, (48, 0),  5)));
            Table.States (146).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 0),  5)));
            Table.States (147).Action_List.Set_Capacity (2);
            Add_Action (Table.States (147), 20, (64, 1), 157);
            Add_Action (Table.States (147), 33, (63, 0), 195);
            Table.States (147).Kernel := To_Vector ((((63, 0),  64,  1, (32767, 0),  0), ((63, 1),  64,  2, (32767, 0),
             0), ((64, 1),  64,  2, (32767, 0),  0)));
            Table.States (147).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (63, 0),  33, 195)));
            Table.States (148).Action_List.Set_Capacity (2);
            Add_Action (Table.States (148), 20, (64, 1), 169);
            Add_Action (Table.States (148), 34, (62, 0), 196);
            Table.States (148).Kernel := To_Vector ((((62, 0),  64,  1, (32767, 0),  0), ((64, 1),  64,  2, (32767, 0),
             0)));
            Table.States (148).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (62, 0),  34, 196)));
         end Subr_3;
         procedure Subr_4
         is begin
            Table.States (149).Action_List.Set_Capacity (2);
            Add_Action (Table.States (149), 20, (64, 1), 181);
            Add_Action (Table.States (149), 35, (61, 0), 197);
            Table.States (149).Kernel := To_Vector ((((61, 0),  64,  1, (32767, 0),  0), ((62, 1),  64,  2, (32767, 0),
             0), ((63, 2),  64,  2, (32767, 0),  0), ((63, 3),  64,  2, (32767, 0),  0), ((64, 1),  64,  2, (32767, 0),
             0)));
            Table.States (149).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (61, 0),  35, 197)));
            Table.States (150).Action_List.Set_Capacity (1);
            Add_Action (Table.States (150), 23, (57, 0), 198);
            Table.States (150).Kernel := To_Vector ((0 => ((57, 0),  39,  3, (32767, 0),  0)));
            Table.States (150).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  23, 198)));
            Table.States (151).Action_List.Set_Capacity (6);
            Add_Action (Table.States (151), 25, (63, 0), 79);
            Add_Action (Table.States (151), 26, (62, 0), 80);
            Add_Action (Table.States (151), 27, (61, 0), 81);
            Add_Action (Table.States (151), 28, (57, 0), 82);
            Add_Action (Table.States (151), 39, (60, 0), 199);
            Add_Action (Table.States (151), 41, (60, 1), 84);
            Table.States (151).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (151), 57, 85);
            Add_Goto (Table.States (151), 60, 200);
            Add_Goto (Table.States (151), 61, 89);
            Add_Goto (Table.States (151), 62, 90);
            Add_Goto (Table.States (151), 63, 91);
            Table.States (151).Kernel := To_Vector ((0 => ((58, 1),  23,  1, (32767, 0),  0)));
            Table.States (151).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (60, 0),  39, 199)));
            Table.States (152).Action_List.Set_Capacity (8);
            Add_Action (Table.States (152), (20, 25, 26, 27, 28, 33, 39, 41), (63, 4),  2);
            Table.States (152).Kernel := To_Vector ((0 => ((63, 4),  31,  0, (63, 4),  2)));
            Table.States (152).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 4),  2)));
            Table.States (153).Action_List.Set_Capacity (8);
            Add_Action (Table.States (153), (20, 25, 26, 27, 28, 33, 39, 41), (62, 2),  2);
            Table.States (153).Kernel := To_Vector ((0 => ((62, 2),  32,  0, (62, 2),  2)));
            Table.States (153).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 2),  2)));
            Table.States (154).Action_List.Set_Capacity (8);
            Add_Action (Table.States (154), (20, 25, 26, 27, 28, 33, 39, 41), (63, 5),  2);
            Table.States (154).Kernel := To_Vector ((0 => ((63, 5),  37,  0, (63, 5),  2)));
            Table.States (154).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 5),  2)));
            Table.States (155).Action_List.Set_Capacity (8);
            Add_Action (Table.States (155), (20, 25, 26, 27, 28, 33, 39, 41), (62, 3),  2);
            Table.States (155).Kernel := To_Vector ((0 => ((62, 3),  32,  0, (62, 3),  2)));
            Table.States (155).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 3),  2)));
            Table.States (156).Action_List.Set_Capacity (8);
            Add_Action (Table.States (156), (20, 25, 26, 27, 28, 33, 39, 41), (59, 1),  2);
            Table.States (156).Kernel := To_Vector ((0 => ((59, 1),  58,  0, (59, 1),  2)));
            Table.States (156).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (59, 1),  2)));
            Table.States (157).Action_List.Set_Capacity (6);
            Add_Action (Table.States (157), 25, (63, 0), 79);
            Add_Action (Table.States (157), 26, (62, 0), 80);
            Add_Action (Table.States (157), 27, (61, 0), 81);
            Add_Action (Table.States (157), 28, (57, 0), 82);
            Add_Action (Table.States (157), 39, (58, 1), 83);
            Add_Action (Table.States (157), 41, (60, 1), 84);
            Table.States (157).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (157), 57, 85);
            Add_Goto (Table.States (157), 58, 86);
            Add_Goto (Table.States (157), 59, 201);
            Add_Goto (Table.States (157), 60, 88);
            Add_Goto (Table.States (157), 61, 89);
            Add_Goto (Table.States (157), 62, 90);
            Add_Goto (Table.States (157), 63, 91);
            Table.States (157).Kernel := To_Vector ((0 => ((64, 1),  20,  1, (32767, 0),  0)));
            Table.States (157).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 83)));
            Table.States (158).Action_List.Set_Capacity (11);
            Add_Action (Table.States (158), 19, Reduce, (63, 0),  3);
            Add_Action (Table.States (158), 20, Reduce, (63, 0),  3);
            Add_Action (Table.States (158), 25, Reduce, (63, 0),  3);
            Add_Action (Table.States (158), 26, Reduce, (63, 0),  3);
            Add_Action (Table.States (158), 27, Reduce, (63, 0),  3);
            Add_Action (Table.States (158), 28, Reduce, (63, 0),  3);
            Add_Action (Table.States (158), 29, (63, 1), 202);
            Add_Action (Table.States (158), 30, Reduce, (63, 0),  3);
            Add_Action (Table.States (158), 36, Reduce, (63, 0),  3);
            Add_Action (Table.States (158), 39, Reduce, (63, 0),  3);
            Add_Action (Table.States (158), 41, Reduce, (63, 0),  3);
            Table.States (158).Kernel := To_Vector ((((63, 0),  33,  0, (63, 0),  3), ((63, 1),  33,  1, (32767, 0),
            0)));
            Table.States (158).Minimal_Complete_Actions := To_Vector (((Reduce, (63, 0),  3), (Shift, (63, 1),  29,
            202)));
            Table.States (159).Action_List.Set_Capacity (2);
            Add_Action (Table.States (159), 20, (64, 1), 157);
            Add_Action (Table.States (159), 33, (63, 0), 203);
            Table.States (159).Kernel := To_Vector ((((63, 0),  64,  1, (32767, 0),  0), ((63, 1),  64,  2, (32767, 0),
             0), ((64, 1),  64,  2, (32767, 0),  0)));
            Table.States (159).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (63, 0),  33, 203)));
            Table.States (160).Action_List.Set_Capacity (2);
            Add_Action (Table.States (160), 20, (64, 1), 169);
            Add_Action (Table.States (160), 34, (62, 0), 204);
            Table.States (160).Kernel := To_Vector ((((62, 0),  64,  1, (32767, 0),  0), ((64, 1),  64,  2, (32767, 0),
             0)));
            Table.States (160).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (62, 0),  34, 204)));
            Table.States (161).Action_List.Set_Capacity (2);
            Add_Action (Table.States (161), 20, (64, 1), 181);
            Add_Action (Table.States (161), 35, (61, 0), 205);
            Table.States (161).Kernel := To_Vector ((((61, 0),  64,  1, (32767, 0),  0), ((62, 1),  64,  2, (32767, 0),
             0), ((63, 2),  64,  2, (32767, 0),  0), ((63, 3),  64,  2, (32767, 0),  0), ((64, 1),  64,  2, (32767, 0),
             0)));
            Table.States (161).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (61, 0),  35, 205)));
            Table.States (162).Action_List.Set_Capacity (1);
            Add_Action (Table.States (162), 23, (57, 0), 206);
            Table.States (162).Kernel := To_Vector ((0 => ((57, 0),  39,  3, (32767, 0),  0)));
            Table.States (162).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  23, 206)));
            Table.States (163).Action_List.Set_Capacity (6);
            Add_Action (Table.States (163), 25, (63, 0), 93);
            Add_Action (Table.States (163), 26, (62, 0), 94);
            Add_Action (Table.States (163), 27, (61, 0), 95);
            Add_Action (Table.States (163), 28, (57, 0), 96);
            Add_Action (Table.States (163), 39, (60, 0), 207);
            Add_Action (Table.States (163), 41, (60, 1), 98);
            Table.States (163).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (163), 57, 99);
            Add_Goto (Table.States (163), 60, 208);
            Add_Goto (Table.States (163), 61, 103);
            Add_Goto (Table.States (163), 62, 104);
            Add_Goto (Table.States (163), 63, 105);
            Table.States (163).Kernel := To_Vector ((0 => ((58, 1),  23,  1, (32767, 0),  0)));
            Table.States (163).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (60, 0),  39, 207)));
            Table.States (164).Action_List.Set_Capacity (8);
            Add_Action (Table.States (164), (20, 25, 26, 27, 28, 34, 39, 41), (63, 4),  2);
            Table.States (164).Kernel := To_Vector ((0 => ((63, 4),  31,  0, (63, 4),  2)));
            Table.States (164).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 4),  2)));
            Table.States (165).Action_List.Set_Capacity (8);
            Add_Action (Table.States (165), (20, 25, 26, 27, 28, 34, 39, 41), (62, 2),  2);
            Table.States (165).Kernel := To_Vector ((0 => ((62, 2),  32,  0, (62, 2),  2)));
            Table.States (165).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 2),  2)));
            Table.States (166).Action_List.Set_Capacity (8);
            Add_Action (Table.States (166), (20, 25, 26, 27, 28, 34, 39, 41), (63, 5),  2);
            Table.States (166).Kernel := To_Vector ((0 => ((63, 5),  37,  0, (63, 5),  2)));
            Table.States (166).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 5),  2)));
            Table.States (167).Action_List.Set_Capacity (8);
            Add_Action (Table.States (167), (20, 25, 26, 27, 28, 34, 39, 41), (62, 3),  2);
            Table.States (167).Kernel := To_Vector ((0 => ((62, 3),  32,  0, (62, 3),  2)));
            Table.States (167).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 3),  2)));
            Table.States (168).Action_List.Set_Capacity (8);
            Add_Action (Table.States (168), (20, 25, 26, 27, 28, 34, 39, 41), (59, 1),  2);
            Table.States (168).Kernel := To_Vector ((0 => ((59, 1),  58,  0, (59, 1),  2)));
            Table.States (168).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (59, 1),  2)));
            Table.States (169).Action_List.Set_Capacity (6);
            Add_Action (Table.States (169), 25, (63, 0), 93);
            Add_Action (Table.States (169), 26, (62, 0), 94);
            Add_Action (Table.States (169), 27, (61, 0), 95);
            Add_Action (Table.States (169), 28, (57, 0), 96);
            Add_Action (Table.States (169), 39, (58, 1), 97);
            Add_Action (Table.States (169), 41, (60, 1), 98);
            Table.States (169).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (169), 57, 99);
            Add_Goto (Table.States (169), 58, 100);
            Add_Goto (Table.States (169), 59, 209);
            Add_Goto (Table.States (169), 60, 102);
            Add_Goto (Table.States (169), 61, 103);
            Add_Goto (Table.States (169), 62, 104);
            Add_Goto (Table.States (169), 63, 105);
            Table.States (169).Kernel := To_Vector ((0 => ((64, 1),  20,  1, (32767, 0),  0)));
            Table.States (169).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 97)));
            Table.States (170).Action_List.Set_Capacity (10);
            Add_Action (Table.States (170), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (62, 0),  3);
            Table.States (170).Kernel := To_Vector ((0 => ((62, 0),  34,  0, (62, 0),  3)));
            Table.States (170).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 0),  3)));
            Table.States (171).Action_List.Set_Capacity (2);
            Add_Action (Table.States (171), 20, (64, 1), 157);
            Add_Action (Table.States (171), 33, (63, 0), 210);
            Table.States (171).Kernel := To_Vector ((((63, 0),  64,  1, (32767, 0),  0), ((63, 1),  64,  2, (32767, 0),
             0), ((64, 1),  64,  2, (32767, 0),  0)));
            Table.States (171).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (63, 0),  33, 210)));
            Table.States (172).Action_List.Set_Capacity (2);
            Add_Action (Table.States (172), 20, (64, 1), 169);
            Add_Action (Table.States (172), 34, (62, 0), 211);
            Table.States (172).Kernel := To_Vector ((((62, 0),  64,  1, (32767, 0),  0), ((64, 1),  64,  2, (32767, 0),
             0)));
            Table.States (172).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (62, 0),  34, 211)));
            Table.States (173).Action_List.Set_Capacity (2);
            Add_Action (Table.States (173), 20, (64, 1), 181);
            Add_Action (Table.States (173), 35, (61, 0), 212);
            Table.States (173).Kernel := To_Vector ((((61, 0),  64,  1, (32767, 0),  0), ((62, 1),  64,  2, (32767, 0),
             0), ((63, 2),  64,  2, (32767, 0),  0), ((63, 3),  64,  2, (32767, 0),  0), ((64, 1),  64,  2, (32767, 0),
             0)));
            Table.States (173).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (61, 0),  35, 212)));
            Table.States (174).Action_List.Set_Capacity (1);
            Add_Action (Table.States (174), 23, (57, 0), 213);
            Table.States (174).Kernel := To_Vector ((0 => ((57, 0),  39,  3, (32767, 0),  0)));
            Table.States (174).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  23, 213)));
            Table.States (175).Action_List.Set_Capacity (6);
            Add_Action (Table.States (175), 25, (63, 0), 107);
            Add_Action (Table.States (175), 26, (62, 0), 108);
            Add_Action (Table.States (175), 27, (61, 0), 109);
            Add_Action (Table.States (175), 28, (57, 0), 110);
            Add_Action (Table.States (175), 39, (60, 0), 214);
            Add_Action (Table.States (175), 41, (60, 1), 112);
            Table.States (175).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (175), 57, 113);
            Add_Goto (Table.States (175), 60, 215);
            Add_Goto (Table.States (175), 61, 117);
            Add_Goto (Table.States (175), 62, 118);
            Add_Goto (Table.States (175), 63, 119);
            Table.States (175).Kernel := To_Vector ((0 => ((58, 1),  23,  1, (32767, 0),  0)));
            Table.States (175).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (60, 0),  39, 214)));
            Table.States (176).Action_List.Set_Capacity (8);
            Add_Action (Table.States (176), (20, 25, 26, 27, 28, 35, 39, 41), (63, 4),  2);
            Table.States (176).Kernel := To_Vector ((0 => ((63, 4),  31,  0, (63, 4),  2)));
            Table.States (176).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 4),  2)));
            Table.States (177).Action_List.Set_Capacity (8);
            Add_Action (Table.States (177), (20, 25, 26, 27, 28, 35, 39, 41), (62, 2),  2);
            Table.States (177).Kernel := To_Vector ((0 => ((62, 2),  32,  0, (62, 2),  2)));
            Table.States (177).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 2),  2)));
            Table.States (178).Action_List.Set_Capacity (8);
            Add_Action (Table.States (178), (20, 25, 26, 27, 28, 35, 39, 41), (63, 5),  2);
            Table.States (178).Kernel := To_Vector ((0 => ((63, 5),  37,  0, (63, 5),  2)));
            Table.States (178).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 5),  2)));
            Table.States (179).Action_List.Set_Capacity (8);
            Add_Action (Table.States (179), (20, 25, 26, 27, 28, 35, 39, 41), (62, 3),  2);
            Table.States (179).Kernel := To_Vector ((0 => ((62, 3),  32,  0, (62, 3),  2)));
            Table.States (179).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 3),  2)));
            Table.States (180).Action_List.Set_Capacity (8);
            Add_Action (Table.States (180), (20, 25, 26, 27, 28, 35, 39, 41), (59, 1),  2);
            Table.States (180).Kernel := To_Vector ((0 => ((59, 1),  58,  0, (59, 1),  2)));
            Table.States (180).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (59, 1),  2)));
            Table.States (181).Action_List.Set_Capacity (6);
            Add_Action (Table.States (181), 25, (63, 0), 107);
            Add_Action (Table.States (181), 26, (62, 0), 108);
            Add_Action (Table.States (181), 27, (61, 0), 109);
            Add_Action (Table.States (181), 28, (57, 0), 110);
            Add_Action (Table.States (181), 39, (58, 1), 111);
            Add_Action (Table.States (181), 41, (60, 1), 112);
            Table.States (181).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (181), 57, 113);
            Add_Goto (Table.States (181), 58, 114);
            Add_Goto (Table.States (181), 59, 216);
            Add_Goto (Table.States (181), 60, 116);
            Add_Goto (Table.States (181), 61, 117);
            Add_Goto (Table.States (181), 62, 118);
            Add_Goto (Table.States (181), 63, 119);
            Table.States (181).Kernel := To_Vector ((0 => ((64, 1),  20,  1, (32767, 0),  0)));
            Table.States (181).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (58, 1),  39, 111)));
            Table.States (182).Action_List.Set_Capacity (13);
            Add_Action (Table.States (182), 19, Reduce, (61, 0),  3);
            Add_Action (Table.States (182), 20, Reduce, (61, 0),  3);
            Add_Action (Table.States (182), 25, Reduce, (61, 0),  3);
            Add_Action (Table.States (182), 26, Reduce, (61, 0),  3);
            Add_Action (Table.States (182), 27, Reduce, (61, 0),  3);
            Add_Action (Table.States (182), 28, Reduce, (61, 0),  3);
            Add_Action (Table.States (182), 30, Reduce, (61, 0),  3);
            Add_Action (Table.States (182), 31, (63, 2), 217);
            Add_Action (Table.States (182), 32, (62, 1), 218);
            Add_Action (Table.States (182), 36, Reduce, (61, 0),  3);
            Add_Action (Table.States (182), 37, (63, 3), 219);
            Add_Action (Table.States (182), 39, Reduce, (61, 0),  3);
            Add_Action (Table.States (182), 41, Reduce, (61, 0),  3);
            Table.States (182).Kernel := To_Vector ((((61, 0),  35,  0, (61, 0),  3), ((62, 1),  35,  1, (32767, 0),
            0), ((63, 2),  35,  1, (32767, 0),  0), ((63, 3),  35,  1, (32767, 0),  0)));
            Table.States (182).Minimal_Complete_Actions := To_Vector (((Reduce, (61, 0),  3), (Shift, (62, 1),  32,
            218), (Shift, (63, 2),  31, 217), (Shift, (63, 3),  37, 219)));
            Table.States (183).Action_List.Set_Capacity (1);
            Add_Action (Table.States (183), 39, (57, 0), 220);
            Table.States (183).Kernel := To_Vector ((0 => ((57, 0),  23,  2, (32767, 0),  0)));
            Table.States (183).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  39, 220)));
            Table.States (184).Action_List.Set_Capacity (13);
            Add_Action (Table.States (184), 19, Reduce, (60, 0),  1);
            Add_Action (Table.States (184), 20, Reduce, (60, 0),  1);
            Add_Action (Table.States (184), 25, Reduce, (60, 0),  1);
            Add_Action (Table.States (184), 26, Reduce, (60, 0),  1);
            Add_Action (Table.States (184), 27, Reduce, (60, 0),  1);
            Add_Action (Table.States (184), 28, Reduce, (60, 0),  1);
            Add_Action (Table.States (184), 30, Reduce, (60, 0),  1);
            Add_Action (Table.States (184), 31, (63, 4), 123);
            Add_Action (Table.States (184), 32, (62, 2), 124);
            Add_Action (Table.States (184), 36, Reduce, (60, 0),  1);
            Add_Action (Table.States (184), 37, (63, 5), 125);
            Add_Action (Table.States (184), 39, Reduce, (60, 0),  1);
            Add_Action (Table.States (184), 41, Reduce, (60, 0),  1);
            Table.States (184).Kernel := To_Vector ((((60, 0),  39,  0, (60, 0),  1), ((62, 2),  39,  1, (32767, 0),
            0), ((63, 4),  39,  1, (32767, 0),  0), ((63, 5),  39,  1, (32767, 0),  0)));
            Table.States (184).Minimal_Complete_Actions := To_Vector (((Reduce, (60, 0),  1), (Shift, (62, 2),  32,
            124), (Shift, (63, 4),  31, 123), (Shift, (63, 5),  37, 125)));
            Table.States (185).Action_List.Set_Capacity (10);
            Add_Action (Table.States (185), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (58, 1),  3);
            Table.States (185).Kernel := To_Vector ((0 => ((58, 1),  60,  0, (58, 1),  3)));
            Table.States (185).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 1),  3)));
            Table.States (186).Action_List.Set_Capacity (3);
            Add_Action (Table.States (186), (30, 39, 42), (54, 0),  5);
            Table.States (186).Kernel := To_Vector ((0 => ((54, 0),  36,  0, (54, 0),  5)));
            Table.States (186).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 0),  5)));
            Table.States (187).Action_List.Set_Capacity (3);
            Add_Action (Table.States (187), (20, 30, 36), (55, 1),  3);
            Table.States (187).Kernel := To_Vector ((0 => ((55, 1),  56,  0, (55, 1),  3)));
            Table.States (187).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 1),  3)));
            Table.States (188).Action_List.Set_Capacity (1);
            Add_Action (Table.States (188), 9, (55, 6), 221);
            Table.States (188).Kernel := To_Vector ((0 => ((55, 6),  7,  1, (32767, 0),  0)));
            Table.States (188).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (55, 6),  9, 221)));
            Table.States (189).Action_List.Set_Capacity (1);
            Add_Action (Table.States (189), 39, (55, 4), 222);
            Table.States (189).Kernel := To_Vector ((((55, 4),  8,  3, (32767, 0),  0), ((55, 5),  8,  3, (32767, 0),
            0)));
            Table.States (190).Action_List.Set_Capacity (1);
            Add_Action (Table.States (190), 39, (55, 2), 223);
            Table.States (190).Kernel := To_Vector ((((55, 2),  9,  3, (32767, 0),  0), ((55, 3),  9,  3, (32767, 0),
            0)));
            Table.States (191).Action_List.Set_Capacity (3);
            Add_Action (Table.States (191), (20, 30, 36), (56, 2),  3);
            Table.States (191).Kernel := To_Vector ((0 => ((56, 2),  19,  0, (56, 2),  3)));
            Table.States (191).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 2),  3)));
            Table.States (192).Action_List.Set_Capacity (3);
            Add_Action (Table.States (192), (30, 39, 42), (48, 4),  6);
            Table.States (192).Kernel := To_Vector ((0 => ((48, 4),  39,  0, (48, 4),  6)));
            Table.States (192).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 4),  6)));
            Table.States (193).Action_List.Set_Capacity (1);
            Add_Action (Table.States (193), 39, (48, 5), 224);
            Table.States (193).Kernel := To_Vector ((0 => ((48, 5),  21,  1, (32767, 0),  0)));
            Table.States (193).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 5),  39, 224)));
            Table.States (194).Action_List.Set_Capacity (1);
            Add_Action (Table.States (194), 39, (50, 1), 225);
            Table.States (194).Kernel := To_Vector ((0 => ((50, 1),  20,  1, (32767, 0),  0)));
            Table.States (194).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 1),  39, 225)));
            Table.States (195).Action_List.Set_Capacity (9);
            Add_Action (Table.States (195), 20, Reduce, (63, 0),  3);
            Add_Action (Table.States (195), 25, Reduce, (63, 0),  3);
            Add_Action (Table.States (195), 26, Reduce, (63, 0),  3);
            Add_Action (Table.States (195), 27, Reduce, (63, 0),  3);
            Add_Action (Table.States (195), 28, Reduce, (63, 0),  3);
            Add_Action (Table.States (195), 29, (63, 1), 226);
            Add_Action (Table.States (195), 33, Reduce, (63, 0),  3);
            Add_Action (Table.States (195), 39, Reduce, (63, 0),  3);
            Add_Action (Table.States (195), 41, Reduce, (63, 0),  3);
            Table.States (195).Kernel := To_Vector ((((63, 0),  33,  0, (63, 0),  3), ((63, 1),  33,  1, (32767, 0),
            0)));
            Table.States (195).Minimal_Complete_Actions := To_Vector (((Reduce, (63, 0),  3), (Shift, (63, 1),  29,
            226)));
            Table.States (196).Action_List.Set_Capacity (8);
            Add_Action (Table.States (196), (20, 25, 26, 27, 28, 33, 39, 41), (62, 0),  3);
            Table.States (196).Kernel := To_Vector ((0 => ((62, 0),  34,  0, (62, 0),  3)));
            Table.States (196).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 0),  3)));
            Table.States (197).Action_List.Set_Capacity (11);
            Add_Action (Table.States (197), 20, Reduce, (61, 0),  3);
            Add_Action (Table.States (197), 25, Reduce, (61, 0),  3);
            Add_Action (Table.States (197), 26, Reduce, (61, 0),  3);
            Add_Action (Table.States (197), 27, Reduce, (61, 0),  3);
            Add_Action (Table.States (197), 28, Reduce, (61, 0),  3);
            Add_Action (Table.States (197), 31, (63, 2), 227);
            Add_Action (Table.States (197), 32, (62, 1), 228);
            Add_Action (Table.States (197), 33, Reduce, (61, 0),  3);
            Add_Action (Table.States (197), 37, (63, 3), 229);
            Add_Action (Table.States (197), 39, Reduce, (61, 0),  3);
            Add_Action (Table.States (197), 41, Reduce, (61, 0),  3);
            Table.States (197).Kernel := To_Vector ((((61, 0),  35,  0, (61, 0),  3), ((62, 1),  35,  1, (32767, 0),
            0), ((63, 2),  35,  1, (32767, 0),  0), ((63, 3),  35,  1, (32767, 0),  0)));
            Table.States (197).Minimal_Complete_Actions := To_Vector (((Reduce, (61, 0),  3), (Shift, (62, 1),  32,
            228), (Shift, (63, 2),  31, 227), (Shift, (63, 3),  37, 229)));
            Table.States (198).Action_List.Set_Capacity (1);
            Add_Action (Table.States (198), 39, (57, 0), 230);
            Table.States (198).Kernel := To_Vector ((0 => ((57, 0),  23,  2, (32767, 0),  0)));
            Table.States (198).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  39, 230)));
            Table.States (199).Action_List.Set_Capacity (11);
            Add_Action (Table.States (199), 20, Reduce, (60, 0),  1);
            Add_Action (Table.States (199), 25, Reduce, (60, 0),  1);
            Add_Action (Table.States (199), 26, Reduce, (60, 0),  1);
            Add_Action (Table.States (199), 27, Reduce, (60, 0),  1);
            Add_Action (Table.States (199), 28, Reduce, (60, 0),  1);
            Add_Action (Table.States (199), 31, (63, 4), 152);
            Add_Action (Table.States (199), 32, (62, 2), 153);
            Add_Action (Table.States (199), 33, Reduce, (60, 0),  1);
            Add_Action (Table.States (199), 37, (63, 5), 154);
            Add_Action (Table.States (199), 39, Reduce, (60, 0),  1);
            Add_Action (Table.States (199), 41, Reduce, (60, 0),  1);
            Table.States (199).Kernel := To_Vector ((((60, 0),  39,  0, (60, 0),  1), ((62, 2),  39,  1, (32767, 0),
            0), ((63, 4),  39,  1, (32767, 0),  0), ((63, 5),  39,  1, (32767, 0),  0)));
            Table.States (199).Minimal_Complete_Actions := To_Vector (((Reduce, (60, 0),  1), (Shift, (62, 2),  32,
            153), (Shift, (63, 4),  31, 152), (Shift, (63, 5),  37, 154)));
         end Subr_4;
         procedure Subr_5
         is begin
            Table.States (200).Action_List.Set_Capacity (8);
            Add_Action (Table.States (200), (20, 25, 26, 27, 28, 33, 39, 41), (58, 1),  3);
            Table.States (200).Kernel := To_Vector ((0 => ((58, 1),  60,  0, (58, 1),  3)));
            Table.States (200).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 1),  3)));
            Table.States (201).Action_List.Set_Capacity (8);
            Add_Action (Table.States (201), 20, Reduce, (64, 1),  3);
            Add_Action (Table.States (201), 25, (63, 0), 79);
            Add_Action (Table.States (201), 26, (62, 0), 80);
            Add_Action (Table.States (201), 27, (61, 0), 81);
            Add_Action (Table.States (201), 28, (57, 0), 82);
            Add_Action (Table.States (201), 33, Reduce, (64, 1),  3);
            Add_Action (Table.States (201), 39, (58, 1), 83);
            Add_Action (Table.States (201), 41, (60, 1), 84);
            Table.States (201).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (201), 57, 85);
            Add_Goto (Table.States (201), 58, 156);
            Add_Goto (Table.States (201), 60, 88);
            Add_Goto (Table.States (201), 61, 89);
            Add_Goto (Table.States (201), 62, 90);
            Add_Goto (Table.States (201), 63, 91);
            Table.States (201).Kernel := To_Vector ((((59, 1),  59,  1, (32767, 0),  0), ((64, 1),  59,  0, (64, 1),
            3)));
            Table.States (201).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (64, 1),  3)));
            Table.States (202).Action_List.Set_Capacity (10);
            Add_Action (Table.States (202), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (63, 1),  4);
            Table.States (202).Kernel := To_Vector ((0 => ((63, 1),  29,  0, (63, 1),  4)));
            Table.States (202).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 1),  4)));
            Table.States (203).Action_List.Set_Capacity (9);
            Add_Action (Table.States (203), 20, Reduce, (63, 0),  3);
            Add_Action (Table.States (203), 25, Reduce, (63, 0),  3);
            Add_Action (Table.States (203), 26, Reduce, (63, 0),  3);
            Add_Action (Table.States (203), 27, Reduce, (63, 0),  3);
            Add_Action (Table.States (203), 28, Reduce, (63, 0),  3);
            Add_Action (Table.States (203), 29, (63, 1), 231);
            Add_Action (Table.States (203), 34, Reduce, (63, 0),  3);
            Add_Action (Table.States (203), 39, Reduce, (63, 0),  3);
            Add_Action (Table.States (203), 41, Reduce, (63, 0),  3);
            Table.States (203).Kernel := To_Vector ((((63, 0),  33,  0, (63, 0),  3), ((63, 1),  33,  1, (32767, 0),
            0)));
            Table.States (203).Minimal_Complete_Actions := To_Vector (((Reduce, (63, 0),  3), (Shift, (63, 1),  29,
            231)));
            Table.States (204).Action_List.Set_Capacity (8);
            Add_Action (Table.States (204), (20, 25, 26, 27, 28, 34, 39, 41), (62, 0),  3);
            Table.States (204).Kernel := To_Vector ((0 => ((62, 0),  34,  0, (62, 0),  3)));
            Table.States (204).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 0),  3)));
            Table.States (205).Action_List.Set_Capacity (11);
            Add_Action (Table.States (205), 20, Reduce, (61, 0),  3);
            Add_Action (Table.States (205), 25, Reduce, (61, 0),  3);
            Add_Action (Table.States (205), 26, Reduce, (61, 0),  3);
            Add_Action (Table.States (205), 27, Reduce, (61, 0),  3);
            Add_Action (Table.States (205), 28, Reduce, (61, 0),  3);
            Add_Action (Table.States (205), 31, (63, 2), 232);
            Add_Action (Table.States (205), 32, (62, 1), 233);
            Add_Action (Table.States (205), 34, Reduce, (61, 0),  3);
            Add_Action (Table.States (205), 37, (63, 3), 234);
            Add_Action (Table.States (205), 39, Reduce, (61, 0),  3);
            Add_Action (Table.States (205), 41, Reduce, (61, 0),  3);
            Table.States (205).Kernel := To_Vector ((((61, 0),  35,  0, (61, 0),  3), ((62, 1),  35,  1, (32767, 0),
            0), ((63, 2),  35,  1, (32767, 0),  0), ((63, 3),  35,  1, (32767, 0),  0)));
            Table.States (205).Minimal_Complete_Actions := To_Vector (((Reduce, (61, 0),  3), (Shift, (62, 1),  32,
            233), (Shift, (63, 2),  31, 232), (Shift, (63, 3),  37, 234)));
            Table.States (206).Action_List.Set_Capacity (1);
            Add_Action (Table.States (206), 39, (57, 0), 235);
            Table.States (206).Kernel := To_Vector ((0 => ((57, 0),  23,  2, (32767, 0),  0)));
            Table.States (206).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  39, 235)));
            Table.States (207).Action_List.Set_Capacity (11);
            Add_Action (Table.States (207), 20, Reduce, (60, 0),  1);
            Add_Action (Table.States (207), 25, Reduce, (60, 0),  1);
            Add_Action (Table.States (207), 26, Reduce, (60, 0),  1);
            Add_Action (Table.States (207), 27, Reduce, (60, 0),  1);
            Add_Action (Table.States (207), 28, Reduce, (60, 0),  1);
            Add_Action (Table.States (207), 31, (63, 4), 164);
            Add_Action (Table.States (207), 32, (62, 2), 165);
            Add_Action (Table.States (207), 34, Reduce, (60, 0),  1);
            Add_Action (Table.States (207), 37, (63, 5), 166);
            Add_Action (Table.States (207), 39, Reduce, (60, 0),  1);
            Add_Action (Table.States (207), 41, Reduce, (60, 0),  1);
            Table.States (207).Kernel := To_Vector ((((60, 0),  39,  0, (60, 0),  1), ((62, 2),  39,  1, (32767, 0),
            0), ((63, 4),  39,  1, (32767, 0),  0), ((63, 5),  39,  1, (32767, 0),  0)));
            Table.States (207).Minimal_Complete_Actions := To_Vector (((Reduce, (60, 0),  1), (Shift, (62, 2),  32,
            165), (Shift, (63, 4),  31, 164), (Shift, (63, 5),  37, 166)));
            Table.States (208).Action_List.Set_Capacity (8);
            Add_Action (Table.States (208), (20, 25, 26, 27, 28, 34, 39, 41), (58, 1),  3);
            Table.States (208).Kernel := To_Vector ((0 => ((58, 1),  60,  0, (58, 1),  3)));
            Table.States (208).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 1),  3)));
            Table.States (209).Action_List.Set_Capacity (8);
            Add_Action (Table.States (209), 20, Reduce, (64, 1),  3);
            Add_Action (Table.States (209), 25, (63, 0), 93);
            Add_Action (Table.States (209), 26, (62, 0), 94);
            Add_Action (Table.States (209), 27, (61, 0), 95);
            Add_Action (Table.States (209), 28, (57, 0), 96);
            Add_Action (Table.States (209), 34, Reduce, (64, 1),  3);
            Add_Action (Table.States (209), 39, (58, 1), 97);
            Add_Action (Table.States (209), 41, (60, 1), 98);
            Table.States (209).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (209), 57, 99);
            Add_Goto (Table.States (209), 58, 168);
            Add_Goto (Table.States (209), 60, 102);
            Add_Goto (Table.States (209), 61, 103);
            Add_Goto (Table.States (209), 62, 104);
            Add_Goto (Table.States (209), 63, 105);
            Table.States (209).Kernel := To_Vector ((((59, 1),  59,  1, (32767, 0),  0), ((64, 1),  59,  0, (64, 1),
            3)));
            Table.States (209).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (64, 1),  3)));
            Table.States (210).Action_List.Set_Capacity (9);
            Add_Action (Table.States (210), 20, Reduce, (63, 0),  3);
            Add_Action (Table.States (210), 25, Reduce, (63, 0),  3);
            Add_Action (Table.States (210), 26, Reduce, (63, 0),  3);
            Add_Action (Table.States (210), 27, Reduce, (63, 0),  3);
            Add_Action (Table.States (210), 28, Reduce, (63, 0),  3);
            Add_Action (Table.States (210), 29, (63, 1), 236);
            Add_Action (Table.States (210), 35, Reduce, (63, 0),  3);
            Add_Action (Table.States (210), 39, Reduce, (63, 0),  3);
            Add_Action (Table.States (210), 41, Reduce, (63, 0),  3);
            Table.States (210).Kernel := To_Vector ((((63, 0),  33,  0, (63, 0),  3), ((63, 1),  33,  1, (32767, 0),
            0)));
            Table.States (210).Minimal_Complete_Actions := To_Vector (((Reduce, (63, 0),  3), (Shift, (63, 1),  29,
            236)));
            Table.States (211).Action_List.Set_Capacity (8);
            Add_Action (Table.States (211), (20, 25, 26, 27, 28, 35, 39, 41), (62, 0),  3);
            Table.States (211).Kernel := To_Vector ((0 => ((62, 0),  34,  0, (62, 0),  3)));
            Table.States (211).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 0),  3)));
            Table.States (212).Action_List.Set_Capacity (11);
            Add_Action (Table.States (212), 20, Reduce, (61, 0),  3);
            Add_Action (Table.States (212), 25, Reduce, (61, 0),  3);
            Add_Action (Table.States (212), 26, Reduce, (61, 0),  3);
            Add_Action (Table.States (212), 27, Reduce, (61, 0),  3);
            Add_Action (Table.States (212), 28, Reduce, (61, 0),  3);
            Add_Action (Table.States (212), 31, (63, 2), 237);
            Add_Action (Table.States (212), 32, (62, 1), 238);
            Add_Action (Table.States (212), 35, Reduce, (61, 0),  3);
            Add_Action (Table.States (212), 37, (63, 3), 239);
            Add_Action (Table.States (212), 39, Reduce, (61, 0),  3);
            Add_Action (Table.States (212), 41, Reduce, (61, 0),  3);
            Table.States (212).Kernel := To_Vector ((((61, 0),  35,  0, (61, 0),  3), ((62, 1),  35,  1, (32767, 0),
            0), ((63, 2),  35,  1, (32767, 0),  0), ((63, 3),  35,  1, (32767, 0),  0)));
            Table.States (212).Minimal_Complete_Actions := To_Vector (((Reduce, (61, 0),  3), (Shift, (62, 1),  32,
            238), (Shift, (63, 2),  31, 237), (Shift, (63, 3),  37, 239)));
            Table.States (213).Action_List.Set_Capacity (1);
            Add_Action (Table.States (213), 39, (57, 0), 240);
            Table.States (213).Kernel := To_Vector ((0 => ((57, 0),  23,  2, (32767, 0),  0)));
            Table.States (213).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  39, 240)));
            Table.States (214).Action_List.Set_Capacity (11);
            Add_Action (Table.States (214), 20, Reduce, (60, 0),  1);
            Add_Action (Table.States (214), 25, Reduce, (60, 0),  1);
            Add_Action (Table.States (214), 26, Reduce, (60, 0),  1);
            Add_Action (Table.States (214), 27, Reduce, (60, 0),  1);
            Add_Action (Table.States (214), 28, Reduce, (60, 0),  1);
            Add_Action (Table.States (214), 31, (63, 4), 176);
            Add_Action (Table.States (214), 32, (62, 2), 177);
            Add_Action (Table.States (214), 35, Reduce, (60, 0),  1);
            Add_Action (Table.States (214), 37, (63, 5), 178);
            Add_Action (Table.States (214), 39, Reduce, (60, 0),  1);
            Add_Action (Table.States (214), 41, Reduce, (60, 0),  1);
            Table.States (214).Kernel := To_Vector ((((60, 0),  39,  0, (60, 0),  1), ((62, 2),  39,  1, (32767, 0),
            0), ((63, 4),  39,  1, (32767, 0),  0), ((63, 5),  39,  1, (32767, 0),  0)));
            Table.States (214).Minimal_Complete_Actions := To_Vector (((Reduce, (60, 0),  1), (Shift, (62, 2),  32,
            177), (Shift, (63, 4),  31, 176), (Shift, (63, 5),  37, 178)));
            Table.States (215).Action_List.Set_Capacity (8);
            Add_Action (Table.States (215), (20, 25, 26, 27, 28, 35, 39, 41), (58, 1),  3);
            Table.States (215).Kernel := To_Vector ((0 => ((58, 1),  60,  0, (58, 1),  3)));
            Table.States (215).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (58, 1),  3)));
            Table.States (216).Action_List.Set_Capacity (8);
            Add_Action (Table.States (216), 20, Reduce, (64, 1),  3);
            Add_Action (Table.States (216), 25, (63, 0), 107);
            Add_Action (Table.States (216), 26, (62, 0), 108);
            Add_Action (Table.States (216), 27, (61, 0), 109);
            Add_Action (Table.States (216), 28, (57, 0), 110);
            Add_Action (Table.States (216), 35, Reduce, (64, 1),  3);
            Add_Action (Table.States (216), 39, (58, 1), 111);
            Add_Action (Table.States (216), 41, (60, 1), 112);
            Table.States (216).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (216), 57, 113);
            Add_Goto (Table.States (216), 58, 180);
            Add_Goto (Table.States (216), 60, 116);
            Add_Goto (Table.States (216), 61, 117);
            Add_Goto (Table.States (216), 62, 118);
            Add_Goto (Table.States (216), 63, 119);
            Table.States (216).Kernel := To_Vector ((((59, 1),  59,  1, (32767, 0),  0), ((64, 1),  59,  0, (64, 1),
            3)));
            Table.States (216).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (64, 1),  3)));
            Table.States (217).Action_List.Set_Capacity (10);
            Add_Action (Table.States (217), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (63, 2),  4);
            Table.States (217).Kernel := To_Vector ((0 => ((63, 2),  31,  0, (63, 2),  4)));
            Table.States (217).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 2),  4)));
            Table.States (218).Action_List.Set_Capacity (10);
            Add_Action (Table.States (218), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (62, 1),  4);
            Table.States (218).Kernel := To_Vector ((0 => ((62, 1),  32,  0, (62, 1),  4)));
            Table.States (218).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 1),  4)));
            Table.States (219).Action_List.Set_Capacity (10);
            Add_Action (Table.States (219), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (63, 3),  4);
            Table.States (219).Kernel := To_Vector ((0 => ((63, 3),  37,  0, (63, 3),  4)));
            Table.States (219).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 3),  4)));
            Table.States (220).Action_List.Set_Capacity (1);
            Add_Action (Table.States (220), 24, (57, 0), 241);
            Table.States (220).Kernel := To_Vector ((0 => ((57, 0),  39,  1, (32767, 0),  0)));
            Table.States (220).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  24, 241)));
            Table.States (221).Action_List.Set_Capacity (3);
            Add_Action (Table.States (221), (20, 30, 36), (55, 6),  4);
            Table.States (221).Kernel := To_Vector ((0 => ((55, 6),  9,  0, (55, 6),  4)));
            Table.States (221).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 6),  4)));
            Table.States (222).Action_List.Set_Capacity (2);
            Add_Action (Table.States (222), 10, (55, 5), 242);
            Add_Action (Table.States (222), 23, (55, 4), 243);
            Table.States (222).Kernel := To_Vector ((((55, 4),  39,  2, (32767, 0),  0), ((55, 5),  39,  2, (32767, 0),
             0)));
            Table.States (223).Action_List.Set_Capacity (2);
            Add_Action (Table.States (223), 10, (55, 3), 244);
            Add_Action (Table.States (223), 23, (55, 2), 245);
            Table.States (223).Kernel := To_Vector ((((55, 2),  39,  2, (32767, 0),  0), ((55, 3),  39,  2, (32767, 0),
             0)));
            Table.States (224).Action_List.Set_Capacity (3);
            Add_Action (Table.States (224), (30, 39, 42), (48, 5),  7);
            Table.States (224).Kernel := To_Vector ((0 => ((48, 5),  39,  0, (48, 5),  7)));
            Table.States (224).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 5),  7)));
            Table.States (225).Action_List.Set_Capacity (4);
            Add_Action (Table.States (225), (20, 30, 39, 42), (50, 1),  3);
            Table.States (225).Kernel := To_Vector ((0 => ((50, 1),  39,  0, (50, 1),  3)));
            Table.States (225).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 1),  3)));
            Table.States (226).Action_List.Set_Capacity (8);
            Add_Action (Table.States (226), (20, 25, 26, 27, 28, 33, 39, 41), (63, 1),  4);
            Table.States (226).Kernel := To_Vector ((0 => ((63, 1),  29,  0, (63, 1),  4)));
            Table.States (226).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 1),  4)));
            Table.States (227).Action_List.Set_Capacity (8);
            Add_Action (Table.States (227), (20, 25, 26, 27, 28, 33, 39, 41), (63, 2),  4);
            Table.States (227).Kernel := To_Vector ((0 => ((63, 2),  31,  0, (63, 2),  4)));
            Table.States (227).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 2),  4)));
            Table.States (228).Action_List.Set_Capacity (8);
            Add_Action (Table.States (228), (20, 25, 26, 27, 28, 33, 39, 41), (62, 1),  4);
            Table.States (228).Kernel := To_Vector ((0 => ((62, 1),  32,  0, (62, 1),  4)));
            Table.States (228).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 1),  4)));
            Table.States (229).Action_List.Set_Capacity (8);
            Add_Action (Table.States (229), (20, 25, 26, 27, 28, 33, 39, 41), (63, 3),  4);
            Table.States (229).Kernel := To_Vector ((0 => ((63, 3),  37,  0, (63, 3),  4)));
            Table.States (229).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 3),  4)));
            Table.States (230).Action_List.Set_Capacity (1);
            Add_Action (Table.States (230), 24, (57, 0), 246);
            Table.States (230).Kernel := To_Vector ((0 => ((57, 0),  39,  1, (32767, 0),  0)));
            Table.States (230).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  24, 246)));
            Table.States (231).Action_List.Set_Capacity (8);
            Add_Action (Table.States (231), (20, 25, 26, 27, 28, 34, 39, 41), (63, 1),  4);
            Table.States (231).Kernel := To_Vector ((0 => ((63, 1),  29,  0, (63, 1),  4)));
            Table.States (231).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 1),  4)));
            Table.States (232).Action_List.Set_Capacity (8);
            Add_Action (Table.States (232), (20, 25, 26, 27, 28, 34, 39, 41), (63, 2),  4);
            Table.States (232).Kernel := To_Vector ((0 => ((63, 2),  31,  0, (63, 2),  4)));
            Table.States (232).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 2),  4)));
            Table.States (233).Action_List.Set_Capacity (8);
            Add_Action (Table.States (233), (20, 25, 26, 27, 28, 34, 39, 41), (62, 1),  4);
            Table.States (233).Kernel := To_Vector ((0 => ((62, 1),  32,  0, (62, 1),  4)));
            Table.States (233).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 1),  4)));
            Table.States (234).Action_List.Set_Capacity (8);
            Add_Action (Table.States (234), (20, 25, 26, 27, 28, 34, 39, 41), (63, 3),  4);
            Table.States (234).Kernel := To_Vector ((0 => ((63, 3),  37,  0, (63, 3),  4)));
            Table.States (234).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 3),  4)));
            Table.States (235).Action_List.Set_Capacity (1);
            Add_Action (Table.States (235), 24, (57, 0), 247);
            Table.States (235).Kernel := To_Vector ((0 => ((57, 0),  39,  1, (32767, 0),  0)));
            Table.States (235).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  24, 247)));
            Table.States (236).Action_List.Set_Capacity (8);
            Add_Action (Table.States (236), (20, 25, 26, 27, 28, 35, 39, 41), (63, 1),  4);
            Table.States (236).Kernel := To_Vector ((0 => ((63, 1),  29,  0, (63, 1),  4)));
            Table.States (236).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 1),  4)));
            Table.States (237).Action_List.Set_Capacity (8);
            Add_Action (Table.States (237), (20, 25, 26, 27, 28, 35, 39, 41), (63, 2),  4);
            Table.States (237).Kernel := To_Vector ((0 => ((63, 2),  31,  0, (63, 2),  4)));
            Table.States (237).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 2),  4)));
            Table.States (238).Action_List.Set_Capacity (8);
            Add_Action (Table.States (238), (20, 25, 26, 27, 28, 35, 39, 41), (62, 1),  4);
            Table.States (238).Kernel := To_Vector ((0 => ((62, 1),  32,  0, (62, 1),  4)));
            Table.States (238).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (62, 1),  4)));
            Table.States (239).Action_List.Set_Capacity (8);
            Add_Action (Table.States (239), (20, 25, 26, 27, 28, 35, 39, 41), (63, 3),  4);
            Table.States (239).Kernel := To_Vector ((0 => ((63, 3),  37,  0, (63, 3),  4)));
            Table.States (239).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (63, 3),  4)));
            Table.States (240).Action_List.Set_Capacity (1);
            Add_Action (Table.States (240), 24, (57, 0), 248);
            Table.States (240).Kernel := To_Vector ((0 => ((57, 0),  39,  1, (32767, 0),  0)));
            Table.States (240).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  24, 248)));
            Table.States (241).Action_List.Set_Capacity (10);
            Add_Action (Table.States (241), (19, 20, 25, 26, 27, 28, 30, 36, 39, 41), (57, 0),  5);
            Table.States (241).Kernel := To_Vector ((0 => ((57, 0),  24,  0, (57, 0),  5)));
            Table.States (241).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 0),  5)));
            Table.States (242).Action_List.Set_Capacity (1);
            Add_Action (Table.States (242), 39, (50, 0), 249);
            Table.States (242).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (242), 50, 250);
            Table.States (242).Kernel := To_Vector ((0 => ((55, 5),  10,  1, (32767, 0),  0)));
            Table.States (242).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  39, 249)));
            Table.States (243).Action_List.Set_Capacity (1);
            Add_Action (Table.States (243), 39, (55, 4), 251);
            Table.States (243).Kernel := To_Vector ((0 => ((55, 4),  23,  1, (32767, 0),  0)));
            Table.States (243).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (55, 4),  39, 251)));
            Table.States (244).Action_List.Set_Capacity (1);
            Add_Action (Table.States (244), 39, (50, 0), 249);
            Table.States (244).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (244), 50, 252);
            Table.States (244).Kernel := To_Vector ((0 => ((55, 3),  10,  1, (32767, 0),  0)));
            Table.States (244).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  39, 249)));
            Table.States (245).Action_List.Set_Capacity (1);
            Add_Action (Table.States (245), 39, (55, 2), 253);
            Table.States (245).Kernel := To_Vector ((0 => ((55, 2),  23,  1, (32767, 0),  0)));
            Table.States (245).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (55, 2),  39, 253)));
            Table.States (246).Action_List.Set_Capacity (8);
            Add_Action (Table.States (246), (20, 25, 26, 27, 28, 33, 39, 41), (57, 0),  5);
            Table.States (246).Kernel := To_Vector ((0 => ((57, 0),  24,  0, (57, 0),  5)));
            Table.States (246).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 0),  5)));
            Table.States (247).Action_List.Set_Capacity (8);
            Add_Action (Table.States (247), (20, 25, 26, 27, 28, 34, 39, 41), (57, 0),  5);
            Table.States (247).Kernel := To_Vector ((0 => ((57, 0),  24,  0, (57, 0),  5)));
            Table.States (247).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 0),  5)));
            Table.States (248).Action_List.Set_Capacity (8);
            Add_Action (Table.States (248), (20, 25, 26, 27, 28, 35, 39, 41), (57, 0),  5);
            Table.States (248).Kernel := To_Vector ((0 => ((57, 0),  24,  0, (57, 0),  5)));
            Table.States (248).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 0),  5)));
            Table.States (249).Action_List.Set_Capacity (3);
            Add_Action (Table.States (249), (20, 30, 36), (50, 0),  1);
            Table.States (249).Kernel := To_Vector ((0 => ((50, 0),  39,  0, (50, 0),  1)));
            Table.States (249).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 0),  1)));
            Table.States (250).Action_List.Set_Capacity (3);
            Add_Action (Table.States (250), 20, (50, 1), 254);
            Add_Conflict (Table.States (250), 20, (55, 5),  6);
            Add_Action (Table.States (250), 30, Reduce, (55, 5),  6);
            Add_Action (Table.States (250), 36, Reduce, (55, 5),  6);
            Table.States (250).Kernel := To_Vector ((((50, 1),  50,  2, (32767, 0),  0), ((55, 5),  50,  0, (55, 5),
            6)));
            Table.States (250).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 5),  6)));
            Table.States (251).Action_List.Set_Capacity (3);
            Add_Action (Table.States (251), (20, 30, 36), (55, 4),  6);
            Table.States (251).Kernel := To_Vector ((0 => ((55, 4),  39,  0, (55, 4),  6)));
            Table.States (251).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 4),  6)));
            Table.States (252).Action_List.Set_Capacity (3);
            Add_Action (Table.States (252), 20, (50, 1), 254);
            Add_Conflict (Table.States (252), 20, (55, 3),  6);
            Add_Action (Table.States (252), 30, Reduce, (55, 3),  6);
            Add_Action (Table.States (252), 36, Reduce, (55, 3),  6);
            Table.States (252).Kernel := To_Vector ((((50, 1),  50,  2, (32767, 0),  0), ((55, 3),  50,  0, (55, 3),
            6)));
            Table.States (252).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 3),  6)));
            Table.States (253).Action_List.Set_Capacity (3);
            Add_Action (Table.States (253), (20, 30, 36), (55, 2),  6);
            Table.States (253).Kernel := To_Vector ((0 => ((55, 2),  39,  0, (55, 2),  6)));
            Table.States (253).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 2),  6)));
            Table.States (254).Action_List.Set_Capacity (1);
            Add_Action (Table.States (254), 39, (50, 1), 255);
            Table.States (254).Kernel := To_Vector ((0 => ((50, 1),  20,  1, (32767, 0),  0)));
            Table.States (254).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 1),  39, 255)));
            Table.States (255).Action_List.Set_Capacity (3);
            Add_Action (Table.States (255), (20, 30, 36), (50, 1),  3);
            Table.States (255).Kernel := To_Vector ((0 => ((50, 1),  39,  0, (50, 1),  3)));
            Table.States (255).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 1),  3)));
         end Subr_5;
      begin
         Subr_1;
         Subr_2;
         Subr_3;
         Subr_4;
         Subr_5;
         Table.Error_Action := new Parse_Action_Node'((Verb => Error, others => <>), null);
      end;

      Table.Error_Recover_Enabled := True;
      Table.McKenzie_Param := McKenzie_Param;
      Table.Max_Parallel := 15;
      return Table;
   end Create_Parse_Table;

   function Create_Productions return WisiToken.Syntax_Trees.Production_Info_Trees.Vector
   is begin
      return Result : WisiToken.Syntax_Trees.Production_Info_Trees.Vector do
         Result.Set_First_Last (43, 66);
         Result (46).Optimized_List := True;
         Result (47).RHSs.Set_First_Last (0, 2);
         Result (47).RHSs (0).In_Parse_Action := null;
         Result (47).RHSs (0).Post_Parse_Action := null;
         Result (47).RHSs (1).In_Parse_Action := null;
         Result (47).RHSs (1).Post_Parse_Action := token_keyword_non_grammar_1'Access;
         Result (47).RHSs (2).In_Parse_Action := null;
         Result (47).RHSs (2).Post_Parse_Action := token_keyword_non_grammar_2'Access;
         Result (48).RHSs.Set_First_Last (0, 12);
         Result (48).RHSs (0).In_Parse_Action := null;
         Result (48).RHSs (0).Post_Parse_Action := declaration_0'Access;
         Result (48).RHSs (1).In_Parse_Action := null;
         Result (48).RHSs (1).Post_Parse_Action := declaration_1'Access;
         Result (48).RHSs (2).In_Parse_Action := null;
         Result (48).RHSs (2).Post_Parse_Action := declaration_2'Access;
         Result (48).RHSs (3).In_Parse_Action := null;
         Result (48).RHSs (3).Post_Parse_Action := declaration_3'Access;
         Result (48).RHSs (4).In_Parse_Action := null;
         Result (48).RHSs (4).Post_Parse_Action := declaration_4'Access;
         Result (48).RHSs (5).In_Parse_Action := null;
         Result (48).RHSs (5).Post_Parse_Action := declaration_5'Access;
         Result (48).RHSs (6).In_Parse_Action := null;
         Result (48).RHSs (6).Post_Parse_Action := declaration_6'Access;
         Result (48).RHSs (7).In_Parse_Action := null;
         Result (48).RHSs (7).Post_Parse_Action := declaration_7'Access;
         Result (48).RHSs (8).In_Parse_Action := null;
         Result (48).RHSs (8).Post_Parse_Action := declaration_8'Access;
         Result (48).RHSs (9).In_Parse_Action := null;
         Result (48).RHSs (9).Post_Parse_Action := declaration_9'Access;
         Result (48).RHSs (10).In_Parse_Action := null;
         Result (48).RHSs (10).Post_Parse_Action := declaration_10'Access;
         Result (48).RHSs (11).In_Parse_Action := null;
         Result (48).RHSs (11).Post_Parse_Action := declaration_11'Access;
         Result (48).RHSs (12).In_Parse_Action := null;
         Result (48).RHSs (12).Post_Parse_Action := declaration_12'Access;
         Result (54).RHSs.Set_First_Last (0, 1);
         Result (54).RHSs (0).In_Parse_Action := null;
         Result (54).RHSs (0).Post_Parse_Action := nonterminal_0'Access;
         Result (54).RHSs (1).In_Parse_Action := null;
         Result (54).RHSs (1).Post_Parse_Action := nonterminal_1'Access;
         Result (55).RHSs.Set_First_Last (0, 6);
         Result (55).RHSs (0).In_Parse_Action := null;
         Result (55).RHSs (0).Post_Parse_Action := rhs_list_0'Access;
         Result (55).RHSs (1).In_Parse_Action := null;
         Result (55).RHSs (1).Post_Parse_Action := rhs_list_1'Access;
         Result (55).RHSs (2).In_Parse_Action := null;
         Result (55).RHSs (2).Post_Parse_Action := rhs_list_2'Access;
         Result (55).RHSs (3).In_Parse_Action := null;
         Result (55).RHSs (3).Post_Parse_Action := rhs_list_3'Access;
         Result (55).RHSs (4).In_Parse_Action := null;
         Result (55).RHSs (4).Post_Parse_Action := rhs_list_4'Access;
         Result (55).RHSs (5).In_Parse_Action := null;
         Result (55).RHSs (5).Post_Parse_Action := rhs_list_5'Access;
         Result (55).RHSs (6).In_Parse_Action := null;
         Result (55).RHSs (6).Post_Parse_Action := rhs_list_6'Access;
         Result (56).RHSs.Set_First_Last (0, 2);
         Result (56).RHSs (0).In_Parse_Action := null;
         Result (56).RHSs (0).Post_Parse_Action := rhs_0'Access;
         Result (56).RHSs (1).In_Parse_Action := null;
         Result (56).RHSs (1).Post_Parse_Action := rhs_1'Access;
         Result (56).RHSs (2).In_Parse_Action := null;
         Result (56).RHSs (2).Post_Parse_Action := rhs_2'Access;
         Result (60).RHSs.Set_First_Last (0, 5);
         Result (60).RHSs (0).In_Parse_Action := null;
         Result (60).RHSs (0).Post_Parse_Action := null;
         Result (60).RHSs (1).In_Parse_Action := null;
         Result (60).RHSs (1).Post_Parse_Action := rhs_item_1'Access;
         Result (60).RHSs (2).In_Parse_Action := null;
         Result (60).RHSs (2).Post_Parse_Action := null;
         Result (60).RHSs (3).In_Parse_Action := null;
         Result (60).RHSs (3).Post_Parse_Action := null;
         Result (60).RHSs (4).In_Parse_Action := null;
         Result (60).RHSs (4).Post_Parse_Action := null;
         Result (60).RHSs (5).In_Parse_Action := null;
         Result (60).RHSs (5).Post_Parse_Action := null;
         Result (62).RHSs.Set_First_Last (0, 3);
         Result (62).RHSs (0).In_Parse_Action := null;
         Result (62).RHSs (0).Post_Parse_Action := null;
         Result (62).RHSs (1).In_Parse_Action := null;
         Result (62).RHSs (1).Post_Parse_Action := null;
         Result (62).RHSs (2).In_Parse_Action := null;
         Result (62).RHSs (2).Post_Parse_Action := null;
         Result (62).RHSs (3).In_Parse_Action := null;
         Result (62).RHSs (3).Post_Parse_Action := rhs_optional_item_3'Access;
         Result (66).RHSs.Set_First_Last (0, 1);
         Result (66).RHSs (0).In_Parse_Action := null;
         Result (66).RHSs (0).Post_Parse_Action := compilation_unit_list_0'Access;
         Result (66).RHSs (1).In_Parse_Action := null;
         Result (66).RHSs (1).Post_Parse_Action := compilation_unit_list_1'Access;
      end return;
   end Create_Productions;

   function Create_Parser
     (Trace      : in WisiToken.Trace_Access;
      User_Data  : in WisiToken.Syntax_Trees.User_Data_Access;
      Language_Fixes                 : in WisiToken.Parse.LR.Parser.Language_Fixes_Access;
      Language_Matching_Begin_Tokens : in WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : in WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access)
     return WisiToken.Parse.LR.Parser.Parser
   is begin
      return Parser : WisiToken.Parse.LR.Parser.Parser do
         Parser.Tree.Lexer := Lexer.New_Lexer (Trace, Wisitoken_Grammar_1_Process_Actions.Descriptor'Access);
         Parser.Productions := Create_Productions;
         Parser.User_Data := User_Data;
         Parser.Partial_Parse_Active := Wisitoken_Grammar_1_Process_Actions.Partial_Parse_Active'Access;
         Parser.Partial_Parse_Byte_Goal := Wisitoken_Grammar_1_Process_Actions.Partial_Parse_Byte_Goal'Access;
         Parser.Table := Create_Parse_Table;
         Parser.Language_Fixes                 := Language_Fixes;
         Parser.Language_Matching_Begin_Tokens := Language_Matching_Begin_Tokens;
         Parser.Language_String_ID_Set         := Language_String_ID_Set;
      end return;
   end Create_Parser;
end Wisitoken_Grammar_1_Process_Main;
