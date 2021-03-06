
unit btadt;

{This file contains a complete implementation of a Binary Tree ADT.

 It also does some memory management by keeping a counter of allocated
 nodes.  This counter is initialized at run time and then incremented
 or decremented in Create_Singleton_BT and Destroy_Singleton_BT.

 All the operations implemented here have names ending in _BT to
 distinguish them from the collection operations.

 NEW: Updated May 31, 1999 to include the Which_Side_BT,
      Get_Balance_Field, and Set_Balance_Field functions.

NEW:  Everything new is marked with the word NEW.  }

interface

{ Main types are TreeElement, BinaryTree and Direction }

type
	TreeElement = string;
	BinaryTree = ^TreeNode;
	TreeNode = record
		      value: TreeElement;
		      Balance: Integer;       {NEW}
		      LeftChild,RightChild,Parent: BinaryTree
		   end;

  { An enumerated type to specify which direction to traverse a tree
    or attach a subtree}
    Direction = (left, right);

procedure Create_Empty_BT(var T: BinaryTree);
procedure Create_Singleton_BT(var T: BinaryTree);
procedure Destroy_BT(var T: BinaryTree);
procedure Destroy_Singleton_BT(var T: BinaryTree);
function Is_Empty_BT(T: BinaryTree): boolean;
procedure Set_Root_Value_BT(T: BinaryTree; V: TreeElement);
function Get_Root_Value_BT(T: BinaryTree): TreeElement;
function LeftChild_BT(T: BinaryTree): BinaryTree;
function RightChild_BT(T: BinaryTree): BinaryTree;
function Parent_BT(T: BinaryTree): BinaryTree;
procedure Prune_BT(T: BinaryTree);
procedure Attach_BT(T1:BinaryTree; T2:BinaryTree; Side:Direction);
function MemCheck_BT: boolean;
function Which_Side_BT(T: BinaryTree): Direction; {NEW}
function Get_Balance_Field_BT(T: BinaryTree): Integer; {NEW}
procedure Set_Balance_Field_BT(T: BinaryTree; B: Integer); {NEW}

implementation

(*************** Special memory management stuff *****************)

(* Note: instead of Get_CNode and Return_CNode, we have memory
   management taking place in Create_Singleton and Destroy_Singleton *)

var MemCount: integer;

function MemCheck_BT: boolean;
begin
   MemCheck_BT := (MemCount = 0);
end;

(*********************** TREE ADT *********************)

(* LeftChild_BT(T)
   Pre: T is a non-empty tree
   Post: returns the left child of T
   NOTE: This function cannot be passed directly to a var parameter.
         So instead of calling MyProc(LeftChild(T)), you would have
         to use TEMP:=LeftChild(T); MyProc(TEMP);.  It may also make
         sense to prune the TEMP before the call, and then reattach
         it afterwards.*)
Function LeftChild_BT;
begin
   LeftChild_BT := T^.LeftChild
end;

(* RightChild_BT(T)
   Pre: T is a non-empty tree
   Post: returns the right child of T
   NOTE: see the note for LeftChild above.*)
Function RightChild_BT;
begin
   RightChild_Bt := T^.RightChild
end;

(* Parent_BT(T)
   Pre: T is a non-empty tree
   Post: returns the Parent of T, or NIL if T has no parent
   NOTE: see the note for LeftChild above.*)
Function Parent_BT;
begin
   Parent_BT := T^.Parent
end;

(* NEW FUNCTION *)
(* Which_Side_BT(T)
   pre: T is a non-empty tree with a parent node
   returns: The side of the parent that T is attached to (i.e. if
            T is the Left Child of its parent, then Which_Side_BT(T)
            will return LEFT *)
Function Which_Side_BT;
begin
   If Parent_BT(T) = NIL then
      writeln('BinaryTreeADT Error: Called Which_Side_BT with no parent node')
   else
      if T=LeftChild_BT(Parent_BT(T)) then
         Which_Side_BT := LEFT
      else
         Which_Side_BT := RIGHT
end;

(* NEW FUNCTION *)
(* Set_Balance_Field_BT(T,B)
   Pre: T is a non-empty tree, B is a balance value
   Post: Places B into the Balance Field of the root node of the tree. *)
Procedure Set_Balance_Field_BT;
begin
   if Is_Empty_BT(T) then
      writeln('BinaryTreeADT Error: Tried to set balance field on empty tree.')
   else
      T^.Balance := B
end;

(* NEW FUNCTION *)
(* Get_Balance_Field_BT(T)
   Pre: T is a non-empty tree
   Post: Returns the Balance Field at the root node of the tree. *)
function Get_Balance_Field_BT;
begin
   if Is_Empty_BT(T) then
      writeln('BinaryTreeADT Error: Tried to get balance field of empty tree.')
   else
      Get_Balance_Field_BT := T^.Balance
end;

(* Is_Empty_BT(T)
   Pre: T is a tree
   Post: returns true if T is empty, false otherwise*)
Function Is_Empty_BT;
begin
     Is_Empty_BT := (T=NIL)
end;

(* Create_Empty_BT(T)
   Pre: none
   Post: creates T as the empty tree *)
Procedure Create_Empty_BT;
begin
     T:=Nil
end;

(* Create_Singleton_BT(T)
   Pre: none
   Post: Creates T as a singleton tree.  Similar to Get_CNode. *)
Procedure Create_Singleton_BT;
begin
   new(T);
   T^.Parent := NIL;
   T^.LeftChild := NIL;
   T^.RightChild := NIL;
   MemCount := MemCount+1  {Also keeps node count for memory management}
end;

(* Set_Root_Value_BT(T,V)
   Pre: T is a non-empty tree, V is a tree element value
   Post: Places value V at the root node of the tree. *)
Procedure Set_Root_Value_BT;
begin
   if Is_Empty_BT(T) then
      writeln('BinaryTreeADT Error: Tried to set root value on empty tree.')
   else
      T^.Value := V
end;

(* Get_Root_Value_BT(T)
   Pre: T is a non-empty tree
   Post: Returns the value at the root node of the tree. *)
function Get_Root_Value_BT;
begin
   if Is_Empty_BT(T) then
      writeln('BinaryTreeADT Error: Tried to get root value of empty tree.')
   else
      Get_Root_Value_BT := T^.value
end;

(* Destroy_Singleton_BT(T)
   Pre: T is a singleton tree
   Post: T is destroyed.  Similar to Return_CNode. *)
Procedure Destroy_Singleton_BT;
begin
   if T=NIL then
      writeln('BinaryTreeADT Error: Destroy_Singleton called with empty tree.')
   else begin
      dispose(T);
      MemCount := MemCount - 1  {Also keeps node count for memory management}
   end
end;

(* Destroy_BT(T)
   Pre: T is a tree
   Post: All contents of T is destroyed *)
Procedure Destroy_BT;
begin
   if not Is_Empty_BT(T) then begin
      Destroy_BT(T^.LeftChild);
      Destroy_Bt(T^.RightChild);
      Destroy_Singleton_BT(T)
   end
end;

(* Prune_BT(T)
   Pre: T is a tree with a parent node
   Post: T is detatched from its parent node. *)
Procedure Prune_BT;
var P: BinaryTree;
begin
   if not Is_Empty_BT(T) then begin
      if T^.Parent=NIL then
         writeln('BinaryTreeADT Error: tried to prune root of tree.')
      else begin
         P := Parent_BT(T);
         if P^.LeftChild = T then
            P^.LeftChild := NIL
         else
            P^.RightChild := NIL;
         T^.Parent := NIL
      end
   end
end;

(* Attach_BT(T1, T2, Side)
   Pre: T1 is a tree, T2 is a non-empty tree, Side is a direction.
        If side is 'left', T2's left child must be empty.
        If side is 'right', T2's right child must be empty.
   Post: T1 is attached to T2 as the left or right subtree, according
         to the value of 'side'. *)
Procedure Attach_BT;
begin
   If ((Side=Left) and not Is_Empty_BT(LeftChild_BT(T2))) or
      ((Side=Right) and not Is_Empty_BT(RightChild_BT(T2))) then
         writeln('BinaryTreeADT Error: tried to join to a non-empty location')
   else if not Is_Empty_BT(T1) then begin
      T1^.Parent := T2;
      if Side=Left then
         T2^.LeftChild := T1
      else
         T2^.RightChild := T1
   end
end;

(************************** Initialization code ******************************)
begin
   MemCount := 0;
end.

