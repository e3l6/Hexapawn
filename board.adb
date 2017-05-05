-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 24 April 2017, CS 442P-003 HW 2
--
-- board.adb
--
-------------------------------------------------------------------------------

with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

package body Board is
   
   -- The caller specifies the SOM to evaluate the new board state so there are
   --   no assumptions for whom it is we want the new state evaluated. Return
   --   True if the new state is a win for specified SOM.
   
   function Evaluate_Win (Side_On_Move : Side_On_Move_Type;
                          Board_State  : Board_State_Type) return Boolean is
      
      Last_Row    : Integer := Board_State.Board_Array'Length(1);
      Last_Column : Integer := Board_State.Board_Array'Length(2);
      
   begin
      if (Side_On_Move = W) then
         for J in 1 .. Last_Column loop
            if (Board_State.Board_Array(Last_Row, J) = 'P') then
               return True;
            end if;
         end loop;
      else
         for J in 1 .. Last_Column loop
            if (Board_State.Board_Array(1, J) = 'p') then
               return True;
            end if;
         end loop;
      end if;
      
      return False;
   end Evaluate_Win;
            
   
   
   ----------------------------------------------------------------------------
   function Move_Piece (Board_State : Board_State_Type;
                        Move        : Move_Type) Return Board_State_Type is
      
      Post_Move_State : Board_State_Type;
      
   begin
      if (Board_State.Side_On_Move = W) then
         -- Test that the piece we want to move is a white piece. Will throw
         --   exception if there is no piece or the piece is black.
         
         if (Board_State.Board_Array(Move.From.R, Move.From.C) /= 'P') then
            raise Illegal_Move with "Origin piece not owned by side on move";
         end if;
         
         -- Change the side on move in the new state
         
         Post_Move_State.Side_On_Move := B;
      else
         -- Test that the piece we want to move is a black piece. Will throw
         --   exception if there is no piece or the piece is black.
         
         if (Board_State.Board_Array(Move.From.R, Move.From.C) /= 'p') then
            raise Illegal_Move with "Origin piece not owned by side on move";
         end if;
         
         -- Change the side on move in the new state
         
         Post_Move_State.Side_On_Move := W;
      end if;
      
      -- Copy the current state to the new state
      
      Post_Move_State.Board_Array :=
        new Board_Array_Type (Board_State.Board_Array'Range(1),
                              Board_State.Board_Array'Range(2));
      Post_Move_State.Board_Array.all := Board_State.Board_Array.all;
      
      -- Move the piece to the new position and clear the new position
      
      Post_Move_State.Board_Array(Move.To.R, Move.To.C) :=
        Board_State.Board_Array(Move.From.R, Move.From.C);
      
      Post_Move_State.Board_Array(Move.From.R, Move.From.C) := '.';
      
      return Post_Move_State;
   end Move_Piece;


   
   ----------------------------------------------------------------------------
   function Move_Scan (Board_State : Board_State_Type) return 
     Move_Vectors.Vector is
      
      Move_List    : Move_Vectors.Vector;
      
   begin
      if (Board_State.Side_On_Move = W) then
         for R in reverse 1 .. Board_State.Board_Array'Length(1) loop
            for C in 1 .. Board_State.Board_Array'Length(2) loop
               if Board_State.Board_Array(R, C) = 'P' then
                  
                  -- Check for capture left
                  
                  if ((R < Board_State.Board_Array'Length(1)) and 
                      (C /= 1)) then
                     if (Board_State.Board_Array(R + 1, C - 1) = 'p') then
                        Move_Vectors.append (Move_List,
                                             ((R, C), (R + 1, C - 1)));
                     end if;
                  end if;
                  
                  -- Check for capture right
                  
                  if ((R < Board_State.Board_Array'Length(1)) and
                      (C < Board_State.Board_Array'Length(2))) then
                     if (Board_State.Board_Array(R + 1, C + 1) = 'p') then
                        Move_Vectors.append (Move_List,
                                             ((R, C), (R + 1, C + 1)));
                     end if;
                  end if;
                  
                  -- Check for advance forward
                  
                  if (R < Board_State.Board_Array'Length(1)) then
                     if (Board_State.Board_Array(R + 1, C) = '.') then
                        Move_Vectors.append (Move_List, ((R, C), (R + 1, C)));
                     end if;
                  end if;
                  
               end if;
            end loop;  -- Col
         end loop;  -- Row
      else  -- Black on move
         for R in 1 .. Board_State.Board_Array'Length(1) loop
            for C in 1 .. Board_State.Board_Array'Length(2) loop
               if Board_State.Board_Array(R, C) = 'p' then
                  
                  -- Check for capture left
                  
                  if ((R > 1) and (C > 1)) then
                     if (Board_State.Board_Array(R - 1, C - 1) = 'P') then
                        Move_Vectors.append (Move_List, ((R, C), (R - 1, C - 1)));
                     end if;
                  end if;
                  
                  -- Check for capture right
                  
                  if ((R > 1) and (C < Board_State.Board_Array'Length(2))) then
                     if (Board_State.Board_Array(R - 1, C + 1) = 'P') then
                        Move_Vectors.append (Move_List, ((R, C), (R - 1, C + 1)));
                     end if;
                  end if;
                  
                  -- Check for advance forward
                  
                  if (R > 1) then
                     if (Board_State.Board_Array(R - 1, C) = '.') then
                        Move_Vectors.append (Move_List, ((R, C), (R - 1, C)));
                     end if;
                  end if;
                  
               end if;
            end loop;  -- Col
         end loop;  -- Row
      end if;  -- White/Black on move
      
      return Move_List;
   end Move_Scan;
   
   
   
   ----------------------------------------------------------------------------
   procedure Play_Game (Board_State : Board_State_Type) is
      
      Game_Won    : Boolean := False;
      
      Move_Cursor : Move_Vectors.Cursor;
      Move_List   : Move_Vectors.Vector;
      
      Next_Board  : Board_State_Type;
      
   begin
      Move_List := Move_Scan (Board_State);
      
      Move_Cursor := Move_Vectors.First (Move_List);
      
      if (not Move_Vectors.Has_Element (Move_Cursor)) then
         Game_Won := False;
      else
         
     Check_Moves:
         while (Move_Vectors.Has_Element (Move_Cursor) and (not Game_Won)) loop
            
            Next_Board := Move_Piece (Board_State,
                                      Move_Vectors.Element (Move_Cursor));
            
            if (Evaluate_Win (Board_State.Side_On_Move,
                              Next_Board) = True) then
               Game_Won := True;
            elsif (Next_Move (Next_Board) = -1) then  -- Negamax inversion here
               Game_Won := True;
               exit Check_moves;
            end if;
            
            Move_Vectors.Next (Move_Cursor);   
         end loop Check_Moves;
         
      end if;
      
      if (Game_Won) then
         Put_Line ("1");
      else
         Put_Line ("-1");
      end if;
      
   end Play_Game;
   
   
   
   ----------------------------------------------------------------------------
   function Next_Move (Board_State : Board_State_Type) return Integer is
      
      Game_Won    : Boolean := False;
      
      Max_Value   : Integer := -1;
      Next_Value  : Integer;
      
      Move_Cursor : Move_Vectors.Cursor;
      Move_List   : Move_Vectors.Vector;
      
      Next_Board  : Board_State_Type;
      
   begin
      Move_List := Move_Scan (Board_State);
      
      Move_Cursor := Move_Vectors.First (Move_List);
      
      if (not Move_Vectors.Has_Element (Move_Cursor)) then
         return -1;
      else
         while (Move_Vectors.Has_Element (Move_Cursor)) loop
            Next_Board := Move_Piece (Board_State,
                                      Move_Vectors.Element (Move_Cursor));
            
            if (Evaluate_Win (Board_State.Side_On_Move,
                              Next_Board) = True) then
               return 1;
            else
               Next_Value := -(Next_Move (Next_Board));  -- Negamax!
               
               if (Next_Value > Max_Value) then
                  Max_Value := Next_Value;
               end if;
            end if;
            
            Move_Vectors.Next (Move_Cursor);
         end loop;
         
         return Max_Value;
      end if;
            
   end Next_Move;
   
   
   
   ----------------------------------------------------------------------------
   procedure Print_Board (Board_State : Board_State_Type) is
   begin
      Put_Line (Standard_Error,
                Side_On_Move_Type'Image (Board_State.Side_On_Move));
      
      for R in reverse 1 .. Board_State.Board_Array'Length(1) loop
         for C in 1 .. Board_State.Board_Array'Length(2) loop
            Put (Standard_Error, Board_State.Board_Array(R, C));
         end loop;  -- Row
         
         New_Line (Standard_Error);
      end loop;  -- Col
      
      New_Line (Standard_Error);
   end Print_Board;
   
end Board;
