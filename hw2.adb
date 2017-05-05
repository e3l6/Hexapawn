------------------------------------------------------------------------------
-- 
-- Eric Laursen, 24 April 2017, CS 442P-003 HW 2
--
-- hw2.adb
--
-------------------------------------------------------------------------------

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Text_IO;          use Ada.Text_IO;

with Board;                use Board;

procedure HW2 is
   
   Side_On_Move      : Character;
   Nr_Rows           : Integer := 0;
   Nr_Columns        : Integer := 0;
      
   Input_Buffer      : String (1 .. Max_Nr_Columns + 1);
   Input_Length      : Integer := 0;
   
   Invalid_Input     : exception;
   
   Input_Board_Array : array (1 .. Max_Nr_Rows, 1 .. Max_Nr_Columns) of
                       Character;
   
   Initial_State     : Board_State_Type;
   
begin
   
   -- We start with input gathering and validation, then send the validated
   --   starting board to the game player.
   
   Get (Side_On_Move);
   
   -- Check to see if the side on move (first line of input) is either black
   --   or white. If not, then input is invalid.
   
   case Side_On_Move is
      when 'B' =>
         Initial_State.Side_On_Move := B;
      when 'W' =>
         Initial_State.Side_On_Move := W;
      when others =>
         raise Invalid_Input with "Invalid side on move";
   end case;            
   
   Skip_Line;  -- Ignore everything up to and including the next new line
   
   -- The first line of the board determines the board width. After than,
   --   we check to make sure that the following lines of the board are
   --   equal in length. If not, then the input is invalid.
   -- Also checking to ensure we've not read more that Max_Nr_Rows of
   --   board input. If we have, then the input is invalid.
   
   Get_Line (Input_Buffer, Nr_Columns);
   Nr_Rows := Nr_Rows + 1;
   
   for I in 1 .. Nr_Columns loop
      Input_Board_Array(Nr_Rows, I) := Input_Buffer(I);
   end loop;
   
   while (not End_Of_File) loop
      Get_Line (Input_Buffer, Input_Length);
      Nr_Rows := Nr_Rows + 1;
      
      if (Input_Length /= Nr_Columns) then
         raise Invalid_Input with "Invalid row length";
      end if;
      
      if (Nr_Rows > Max_Nr_Rows) then
         raise Invalid_Input with "Invalid number of rows";
      end if;
      
      for I in 1 .. Nr_Columns loop
         Input_Board_Array(Nr_Rows, I) := Input_Buffer(I);
      end loop; -- for I
   end loop;
   
   -- We need a minimum of 3 rows of board, so if it's less than that,
   --   yup, the board in invalid. Otherwise, if the input is valid up
   --   until now, then validate the input characters and populate an
   --   appropriately sized game board array (reversing the input board so that
   --   the game board is oriented with (1,1) in the lower left corner).
   
   if (Nr_Rows < 3) then
      raise Invalid_Input with "Invalid number of rows";
   end if;
   
   Initial_State.Board_Array := new Board_Array_Type (1 .. Nr_Rows, 1 .. Nr_Columns);
   
    for I in 1 .. Nr_Rows loop
       for J in 1 .. Nr_Columns loop
          case Input_Board_Array(I, J) is
             when 'P' | 'p' | '.' =>
                Initial_State.Board_Array(Nr_Rows - I + 1, J) := Input_Board_Array(I, J);
             when others =>
                raise Invalid_Input with "Invalid board character input";
          end case;
       end loop;  -- column
    end loop;  -- row
       
    Play_Game (Initial_State);
end HW2;
