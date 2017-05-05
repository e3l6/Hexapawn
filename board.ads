-------------------------------------------------------------------------------
-- 
-- Eric Laursen, 24 April 2017, CS 442P-003 HW 2
--
-- board.ads
--
-------------------------------------------------------------------------------

with Ada.Containers.Vectors;  use Ada.Containers;

package Board is
   
   Max_Nr_Columns   : constant Integer := 8;
   Max_Nr_Rows      : constant Integer := 8;
   
   Illegal_Move     : exception;
   
   
   
   ----------------------------------------------------------------------------
   type Board_Array_Type is array (Positive range <>, Positive range <>) of
     Character;
   
   type Board_Position_Type is record
      R, C : Integer;  -- Row, Column
   end record;
   
   type Move_Type is record
      From, To : Board_Position_Type;
   end record;      
   
   type Side_On_Move_Type is (W, B);  -- W for white, B for black
   
   type Board_State_Type is record
      Side_On_Move : Side_On_Move_Type;
      Board_Array  : access Board_Array_Type;
   end record;
   
   
   
   ----------------------------------------------------------------------------
   package Move_Vectors is new Vectors (Positive, Move_Type);
   
   
   ----------------------------------------------------------------------------
   function  Evaluate_Win (Side_On_Move : Side_On_Move_Type;
                           Board_State  : Board_State_Type) return Boolean;
   
   function  Move_Piece   (Board_State : Board_State_Type;
                           Move       : Move_Type) return Board_State_Type;
   
   function  Move_Scan    (Board_State : Board_State_Type)
                          return Move_Vectors.Vector;
   
   procedure Play_Game    (Board_State : Board_State_Type);
   
   function  Next_Move    (Board_State : Board_State_Type) return Integer;
   
   procedure Print_Board  (Board_State : Board_State_Type);
   
end Board;
