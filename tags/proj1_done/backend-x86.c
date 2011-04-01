/*
 *
 *   backend-x86.c
 *
 *   backend functions for compiler construction
 *
 */



#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "defs.h"
#include "types.h"
#include "message.h"
/* defined in defs.h */
#include BACKEND_HEADER_FILE

/* Override external b_pop() definition with internal one. */
#undef   b_pop
#define  b_pop()  b_internal_pop(FALSE)


#define errfp stderr
#define outfp stdout       /* outfp is the file to which emit and emitn
			      send their code.  Set it to stdout for
			      now.  */


/* Stack to count the number of words taken by actual arguments to a
   function call.  A stack is necessary because function calls may be
   nested. */
static int actual_arg_word_count[MAX_CALL_NEST];
/* Stack to count the space used by actual arguments to a function call.
   This stack grows and shrinks along with actual_arg_word_count, so
   use aa_top as the common top index. */
static int actual_arg_space[MAX_CALL_NEST];
static int aa_top = -1;

/* These vars give various offsets from %fp:
       return_value_offset - where the return value of the function
                           can be stored and updated
       double_base_offset - where double formal params are stored in
                            callee's frame
       caller_offset - where non-double formal params are stored
                       (in caller's frame)
       loc_var_offset - offsets of local vars

   They are all initialized in b_func_prologue() and updated in
   b_store_formal_params(), except return_val_offset,
   which is initialized in b_alloc_return_value() and never updated.
   loc_var_offset is also updated as local variables
   are allocated.

   In Pascal, procedure/function definitions can be nested, so the user
   must be careful not to call b_func_prologue for a proc/func until
   all local procs/funcs have been processed.
*/
static int return_value_offset = 0;	/* Guaranteed illegal value */
static int double_base_offset;
static int caller_offset;
static int loc_var_offset;

/* Gives the register number from which to retrieve formal parameters.  It is
   initialized in b_func_prologue() and updated in b_store_formal_params().
*/
static int formal_reg_no;


/* asm_section keeps track of the current section in the assembler. */
static ASM_SECTION asm_section = SEC_NONE;


/* Each flag prevents the given floating point constant from being
   allocated twice. */
static int float_one_allocated = FALSE;
static int double_one_allocated = FALSE;
static int double_zero_allocated = FALSE;


/* Temporary locations for storing floats and doubles */
static float global_float_val;
static double global_double_val;


/* Function to calculate the least multiple of m that is >= x.
   Assumes m>0 and x>=0. */
static int next_multiple(int x, int m)
{
  x += m-1;
  return x - x%m;
}



/* Assumes the difference (in bytes) between two addresses is in %o0.  Emits
   code to divide this value by the given size parameter.  The result is left
   in %o0.  This function is used to compute the integer difference of two
   pointers to data of the given size.  The value initially in %o0 must be a
   (positive or negative) multiple of the given size; otherwise, the results
   are not what one might expect.  The algorithm essentially multiplies %o0
   by the inverse of size (mod 2^{32}).  To ensure that size is odd, both
   size and %o0 are arithmetically right-shifted beforehand. */
static void divide_by_size(unsigned int size);



/* Makes room on the stack for a temporary value */
static void b_push()
{
  emit ("	sub	%%sp, 8, %%sp");
}



/* Removes and discards a value from the top of the stack.  If passed TRUE,
   a comment is placed in the assembly code.  This function is used, for
   example, to discard the return value in an assignment statement or
   function call. */

void b_internal_pop (BOOLEAN display_flag)
{
  if (display_flag)
    emit ("\t\t\t\t# b_pop ()");

  emit ("	add	%%sp, 8, %%sp");
}



#if 0
/* This is not available in SPARC assembler. */

/* Generates a .err assembler directive, which tells the assembler to
   stop assembling. */

void b_error (void)
{
  emit ("	.err");
}
#endif



/* b_jump accepts a label and emits an unconditional jump to
   that label.  */


void b_jump (char *label)
{
  emit ("\t\t\t\t# b_jump ( destination = %s )", label);

  emit ("	b	%s", label);
  emit ("	nop");
}






/* b_cond_jump accepts a TYPETAG, a B_COND (B_ZERO or B_NONZERO),
   and a label.  It assumes that there is a value of type "type"
   on the stack and emits code that pops the value off the stack
   and does a conditional jump based on that value and the B_COND
   supplied.  For example, calling b_cond_jump with the arguments
   TYSIGNEDINT, B_ZERO, and ".L1" generates code that pops an 
   integer off the stack and checks the value.  If it is zero it
   jumps to ".L1", otherwise it does not jump.

   Note:  The function "new_symbol" is the source of new labels.
          Every time you call new_symbol you get a new label.

   Note:  See function b_dispatch for a different type of
          conditional jump.  */


void b_cond_jump (TYPETAG type, B_COND cond, char *label)
{
  char *temp_label;

  emitn ("\t\t\t\t# b_cond_jump (");
  my_print_typetag (type);
  emit (", %s,", cond == B_ZERO ? "ZERO" : "NON-ZERO");
  emit  ("\t\t\t\t#              %s)", label);

  switch (type) {

  case TYSIGNEDCHAR:
  case TYUNSIGNEDCHAR:
  case TYSIGNEDINT:
  case TYUNSIGNEDINT:
  case TYSIGNEDLONGINT:
  case TYUNSIGNEDLONGINT:
  case TYPTR:
    emit ("	ld%s	[%%sp], %%o0",
	  (type == TYSIGNEDCHAR || type == TYUNSIGNEDCHAR) ? "sb" : "");
    b_pop ();
    emit ("	tst	%%o0");

    /* skipnz is not recognized by gcc */
#if 0
    emit ("	skip%sz", cond == B_ZERO ? "n" : "");
#else
    emit ("	b%sz,a	.+8", cond == B_ZERO ? "" : "n");
#endif
    b_jump (label);
    break;

  case TYDOUBLE:
      temp_label = new_symbol();
    emit ("	ldd	[%%sp], %%f2");
    b_pop ();
    if (!double_zero_allocated) {
	if (asm_section != SEC_TEXT)
	    bug("non-text assembler section in b_cond_jump");
	emit ("	.section	\".rodata\"");
	emit ("	.align	8");
	b_label (".Ld0");
	b_alloc_double(0.0);
	emit ("	.section	\".text\"");
	double_zero_allocated = TRUE;
    }
    emit ("	set	.Ld0, %%o0");
    emit ("	ldd	[%%o0], %%f4");
    emit ("	fcmpd	%%f2, %%f4");
    emit ("	nop");
    emit ("	fb%se	%s", cond == B_ZERO? "n": "", temp_label);
    emit ("	nop");
    b_jump (label);
    b_label (temp_label);
    break;

  default:
    fatal ("b_cond_jump");
  }
}






/* b_dispatch accepts a relational operator, a type, an integer
   comparison value, and a label.  The operator must be either B_EQ,
   B_NE, B_LT, B_LE, B_GT, or B_GE.  The type must be either
   TYSIGNEDINT, TYUNSIGNEDINT, TYSIGNEDLONGINT, or TYUNSIGNEDLONGINT.
   It assumes that there is a value on the stack of the given type,
   and emits code that compares that value (left) with the cmp_calue
   (right).  If the two values do NOT satisfy the relation, then no
   jump is executed and the stack value is left on the stack.  If the
   two values DO satisfy the relation, then the stack value is either
   popped off the stack (if pop_on_jump) or left on the stack (if not
   pop_on_jump), and the jump is executed.  For example, calling
   b_dispatch with the arguments B_EQ, TYSIGNEDINT, 45, ".L1", and TRUE
   generates code that compares the integer value on the top of the stack
   with 45.  If the two are not equal, no jump is executed and the stack
   value is left on the stack.  If the two values are equal, the stack 
   value is popped off the stack and a jump to ".L1" is generated.

   Note:  The function "new_symbol" is the source of new labels.
          Every time you call new_symbol you get a new label. 

   Note:  See function b_cond_jump for a different type
          of conditional jump.  */


void b_dispatch (B_ARITH_REL_OP op, TYPETAG type, int cmp_value, char *label,
		 BOOLEAN pop_on_jump)
{
  char *temp_label = new_symbol();
  char *usuffix, *brsuffix;

  emitn ("\t\t\t\t# b_dispatch ( %s,", b_arith_rel_op_string(op));
  my_print_typetag (type);
  emit  (", %d, %s, %s )", cmp_value, label,
	 pop_on_jump ? "pop on jump" : "no pop on jump");

  switch (type) {
  case TYSIGNEDINT:
  case TYSIGNEDLONGINT:
      usuffix = "";
      break;
  case TYUNSIGNEDINT:
  case TYUNSIGNEDLONGINT:
      usuffix = "u";
      break;
  default:
      bug ("unsupported type in b_dispatch");
  }

  /* Branch suffix has the opposite sense of op, since we are branching
     around an unconditional jump. */
  switch (op) {
  case B_EQ:
      brsuffix = "ne";
      usuffix = "";	/* no unsigned version of this branch */
      break;
  case B_NE:
      brsuffix = "e";
      usuffix = "";	/* no unsigned version of this branch */
      break;
  case B_LT:
      brsuffix = "ge";
      break;
  case B_LE:
      brsuffix = "g";
      break;
  case B_GT:
      brsuffix = "le";
      break;
  case B_GE:
      brsuffix = "l";
      break;
  default:
      bug("b_dispatch: illegal comparison operator: %s",
	  b_arith_rel_op_string(op));
  }

  emit ("	ld	[%%sp], %%o0");
  emit ("	set	%d, %%o1", cmp_value);
  emit ("	cmp	%%o0, %%o1");
  emit ("	b%s%s	%s", brsuffix, usuffix, temp_label);
  emit ("	nop");
  if (pop_on_jump)
      b_pop ();
  b_jump (label);
  b_label (temp_label);
#if 0
  if (!pop_on_jump)
      b_pop ();
#endif
}





/* b_duplicate pushes a duplicate of the datum currently on the stack.
   The datum is assumed to be of the given type.  */


void b_duplicate (TYPETAG type)
{
  emitn ("\t\t\t\t# b_duplicate (");
  my_print_typetag(type);
  emit (")");

  switch (type) {

  case TYSIGNEDCHAR:
  case TYUNSIGNEDCHAR:
    emit ("	ldsb	[%%sp], %%i0");
    b_push ();
    emit ("	stb	%%i0, [%%sp]");
    break;

  case TYSIGNEDSHORTINT:
  case TYUNSIGNEDSHORTINT:
    emit ("	ldsh	[%%sp], %%i0");
    b_push ();
    emit ("	sth	%%i0, [%%sp]");
    break;

  case TYSIGNEDINT:
  case TYUNSIGNEDINT:
  case TYSIGNEDLONGINT:
  case TYUNSIGNEDLONGINT:
  case TYPTR:
    emit ("	ld	[%%sp], %%i0");
    b_push ();
    emit ("	st	%%i0, [%%sp]");
    break;

  case TYFLOAT:
    emit ("	ld	[%%sp], %%f2");
    b_push ();
    emit ("	st	%%f2, [%%sp]");
    break;

  case TYDOUBLE:
    emit ("	ldd	[%%sp], %%f2");
    b_push ();
    emit ("	std	%%f2, [%%sp]");
    break;

  default:
    bug ("unsupported type in b_duplicate");
  }
}






/* b_push_ext_addr accepts a global variable name and emits 
   code to push the address of that variable onto the stack.  */


void b_push_ext_addr (char *id)
{
  emit ("\t\t\t\t# b_push_ext_addr (%s)", id);

  emit ("	set	%s, %%i0", id);
  b_push ();
  emit ("	st	%%i0, [%%sp]");
}





/* Added to unify assignment of local and global variables. -SF 2/3/96 */

/* b_push_loc_addr accepts an offset value (from the frame pointer)
   as parameter, and emits code to push the effective address offset($fp)
   onto the stack.  This is what you would call to get the actual address
   of a parameter or local variable onto the stack, given the offset
   value for the variable. */


void b_push_loc_addr (int offset)
{
  emit ("\t\t\t\t# b_push_loc_addr (offset = %d)", offset);

  emit ("	set	%d, %%i0", offset);
  emit ("	add	%%fp, %%i0, %%i0");
  b_push ();
  emit ("	st	%%i0, [%%sp]");
}




/* b_offset accepts an offset value as a parameter, and assumes some
   address is currently on the stack.  It pops the address and pushes
   the result obtained by adding the offset to the address.  This is
   useful both for finding members in structs and for following reference
   links in Pascal.  */


void b_offset (int offset)
{
  emit ("\t\t\t\t# b_offset (offset = %d)", offset);

  emit ("	ld	[%%sp], %%i0");
  emit ("	set	%d, %%i2", offset);
  emit ("	add	%%i0, %%i2, %%i0");
  emit ("	st	%%i0, [%%sp]");
}





/* b_deref accepts a type.  It assumes that the address of
   a variable of that type is on the stack.  It pops the
   address and pushes the value stored at that address
   onto the stack.   */


void b_deref (TYPETAG type)
{
  emitn ("\t\t\t\t# b_deref (");
  my_print_typetag (type);
  emit (")");

  emit ("	ld	[%%sp], %%i0");

  switch (type) {

  case TYSIGNEDCHAR:
  case TYUNSIGNEDCHAR:
    emit ("	ldsb	[%%i0], %%i0");
    emit ("	stb	%%i0, [%%sp]");
    break;

  case TYSIGNEDSHORTINT:
  case TYUNSIGNEDSHORTINT:
    emit ("	ldsh	[%%i0], %%i0");
    emit ("	sth	%%i0, [%%sp]");
    break;

  case TYSIGNEDINT:
  case TYUNSIGNEDINT:
  case TYSIGNEDLONGINT:
  case TYUNSIGNEDLONGINT:
  case TYPTR:
    emit ("	ld	[%%i0], %%i0");
    emit ("	st	%%i0, [%%sp]");
    break;

  case TYFLOAT:
    emit ("	ld	[%%i0], %%f2");
    emit ("	st	%%f2, [%%sp]");
    break;

  case TYDOUBLE:
    emit ("	ldd	[%%i0], %%f2");
    emit ("	std	%%f2, [%%sp]");
    break;

  default:
    bug ("unsupported type in b_deref");
  }
}





/* b_push_const_int accepts an integer value and emits code to
   push that value onto the stack.  */


void b_push_const_int (int value)
{
  emit ("\t\t\t\t# b_push_const_int (%d)", value);

  emit ("	set	%d, %%i0", value);
  b_push ();
  emit ("	st	%%i0, [%%sp]");
}






/* b_push_const_double accepts a double value and emits code to
   push that value onto the stack.  It does this by generating a new 
   label for the string, inserting a labeled double value (two .long's)
   into the .rodata section, and pushing the 8-byte value at the label 
   onto the stack.  */


void b_push_const_double (double value)
{
  char *label;

  if (asm_section != SEC_TEXT)
    bug("non-text assembler section in b_push_const_double");

  emit ("\t\t\t\t# b_push_const_double (%.16e)", value);
  
  emit ("	.section	\".rodata\"");
  emit ("	.align	8");
  b_label (label = new_symbol());
  b_alloc_double (value);
  emit ("	.section	\".text\"");
  emit ("	set	%s, %%i0", label);
  emit ("	ldd	[%%i0], %%f2");
  b_push ();
  emit ("	std	%%f2, [%%sp]");
}






/* b_push_const_string accepts a string and emits code to "push
   the string onto the stack."  It does this by generating a new 
   label for the string, inserting a labeled line of ascii text 
   into the .rodata section, and pushing the address of the label 
   onto the stack.  */


void b_push_const_string (char *string)
{
  char *label;

  emit ("\t\t\t\t# b_push_const_string (\"%s\")", string);

  if (asm_section != SEC_TEXT)
    bug("non-text assembler section in b_push_const_string");

  emit ("	.section	\".rodata\"");
  emit("	.align	4");
  b_label (label = new_symbol());
  emit ("	.asciz	\"%s\"", string);
  emit ("	.section	\".text\"");
  b_push_ext_addr (label);
}






/* Changed name of next procedure to b_assign. -SF 2/3/96 */

/* b_assign accepts a type and emits code to store a value of that
   type in a variable OF THE SAME TYPE.  It assumes that a value of 
   that type is at the top of the stack and that the address of the 
   variable is the next item on the stack.  It pops both values off 
   the stack, stores the value at the address, AND PUSHES THE VALUE
   BACK ONTO THE STACK.  In other words, this is the code you would need 
   for assigning a value to a variable.  Note that it is assumed that the
   stack contains the actual address of the object, so for local variables
   and parameters, you must obtain the actual address beforehand using
   b_push_loc_addr.  */


void b_assign (TYPETAG type)
{
  char *op_suffix = "";

  emitn ("\t\t\t\t# b_assign (");
  my_print_typetag (type);
  emit (")");

  switch (type) {
  case TYSIGNEDCHAR:
  case TYUNSIGNEDCHAR:
      op_suffix = "sb";
      break;
  case TYSIGNEDINT:
  case TYUNSIGNEDINT:
  case TYSIGNEDLONGINT:
  case TYUNSIGNEDLONGINT:
  case TYPTR:
      break;
  case TYFLOAT:
      break;
  case TYDOUBLE:
      op_suffix = "d";
      break;
  default:
    bug ("unsupported type in b_assign");
  }

  emit ("	ld%s	[%%sp], %%i0", op_suffix);
  b_pop ();
  emit ("	ld	[%%sp], %%i2");    
  emit ("	st%s	%%i0, [%%i2]", op_suffix);
  emit ("	st%s	%%i0, [%%sp]", op_suffix);
}





/* b_convert accepts a from_type and a to_type and emits code to
   convert a value of type from_type to a value of type to_type.
   It assumes that there is a value of type from_type on the 
   stack.  That value is popped off the stack, converted to a value 
   of the to_type, and pushed back onto the stack.  */


void b_convert (TYPETAG from_type, TYPETAG to_type)
{
  emitn ("\t\t\t\t# b_convert (");
  my_print_typetag (from_type);
  emitn (" -> ");
  my_print_typetag (to_type);
  emit (")");

  switch (from_type) {

  case TYSIGNEDCHAR:
  case TYUNSIGNEDCHAR:

      emit ("	ld%sb	[%%sp], %%i0", from_type==TYSIGNEDCHAR ? "s" : "u");
      emit ("	st	%%i0, [%%sp]");

      switch (to_type) {
      case TYSIGNEDINT:
      case TYUNSIGNEDINT:
      case TYSIGNEDLONGINT:
      case TYUNSIGNEDLONGINT:
	  break;		/* No alteration of data is necessary */
      case TYFLOAT:
	  emit ("	ld	[%%sp], %%f2");
	  emit ("	fitos	%%f2, %%f2");
	  emit ("	st	%%f2, [%%sp]");
	  break;
      case TYDOUBLE:
	  emit ("	ld	[%%sp], %%f2");
	  emit ("	fitod	%%f2, %%f2");
	  emit ("	std	%%f2, [%%sp]");
	  break;
      default:
	  bug ("unsupported conversion type in b_convert");
      }
      break;

  case TYSIGNEDINT:
  case TYUNSIGNEDINT:
  case TYSIGNEDLONGINT:
  case TYUNSIGNEDLONGINT:

      switch (to_type) {
      case TYSIGNEDCHAR:
      case TYUNSIGNEDCHAR:
	  emit ("	ld	[%%sp], %%i0");
	  emit ("	stb	%%i0, [%%sp]");
	  break;
      case TYPTR:
	  break;		/* No alteration of data necessary */
      case TYFLOAT:
	  emit ("	ld	[%%sp], %%f2");
	  emit ("	fitos	%%f2, %%f2");
	  emit ("	st	%%f2, [%%sp]");
	  break;
      case TYDOUBLE:
	  emit ("	ld	[%%sp], %%f2");
	  emit ("	fitod	%%f2, %%f2");
	  emit ("	std	%%f2, [%%sp]");
	  break;
      default:
	  bug ("unsupported conversion type in b_convert");
      }
      break;

  case TYFLOAT:
      emit ("	ld	[%%sp], %%f2");
    
      switch (to_type) {
      case TYSIGNEDCHAR:
      case TYUNSIGNEDCHAR:
	  emit ("	fstoi	%%f2, %%f2");
	  emit ("	st	%%f2, [%%sp]");
	  emit ("	ld	[%%sp], %%i0");
	  emit ("	stb	%%i0, [%%sp]");
	  break;
      case TYSIGNEDINT:
      case TYUNSIGNEDINT:
      case TYSIGNEDLONGINT:
      case TYUNSIGNEDLONGINT:
	  emit ("	fstoi	%%f2, %%f2");
	  emit ("	st	%%f2, [%%sp]");
	  break;
      case TYDOUBLE:
	  emit ("	fstod	%%f2, %%f2");
	  emit ("	std	%%f2, [%%sp]");
	  break;
      default:
	  bug ("unsupported conversion type in b_convert");
      }
      break;

  case TYDOUBLE:
      emit ("	ldd	[%%sp], %%f2");

      switch (to_type) {
      case TYSIGNEDCHAR:
      case TYUNSIGNEDCHAR:
	  emit ("	fdtoi	%%f2, %%f2");
	  emit ("	st	%%f2, [%%sp]");
	  emit ("	ld	[%%sp], %%i0");
	  emit ("	stb	%%i0, [%%sp]");
	  break;
      case TYSIGNEDINT:
      case TYUNSIGNEDINT:
      case TYSIGNEDLONGINT:
      case TYUNSIGNEDLONGINT:
	  emit ("	fdtoi	%%f2, %%f2");
	  emit ("	st	%%f2, [%%sp]");
	  break;
      case TYFLOAT:
	  emit ("	fdtos	%%f2, %%f2");
	  emit ("	std	%%f2, [%%sp]");
	  break;
      default:
	  bug ("unsupported conversion type in b_convert");
      }
      break;

  }
}






/* b_negate accepts a type and emits code to negate a value of 
   that type.  It assumes a value of that type is on the stack.
   It pops that value off the stack, negates it, and pushes it
   back onto the stack.  */


void b_negate (TYPETAG type)
{
  emitn ("\t\t\t\t# b_negate (");
  my_print_typetag (type);
  emit (")");

  switch (type) {

  case TYSIGNEDINT:
  case TYUNSIGNEDINT:
  case TYSIGNEDLONGINT:
  case TYUNSIGNEDLONGINT:
    emit ("	ld	[%%sp], %%i0");
    emit ("	neg	%%i0");
    emit ("	st	%%i0, [%%sp]");
    break;
    
  case TYDOUBLE:
    emit ("	ldd	[%%sp], %%f2");
    emit ("	fnegs	%%f2, %%f2");
    emit ("	std	%%f2, [%%sp]");
    break;

  default:
    bug ("unsupported type in b_negate");
  }
}




/* Changed to treat uniformly global and local variables, and to
   allow arbitrary l-values (not just id's).  -SF 2/3/96 */

/* b_inc_dec accepts a type, an increment-decrement operator
   (B_PRE_INC, B_POST_INC, B_PRE_DEC, B_POST_DEC), and a size
   parameter.  It emits code to do the indicated
   increment-decrement operation on a variable of the indicated
   type.  It is assumed that a pointer (l-value) is on top of
   the stack.  The function emits code to pop the pointer off the
   stack, increment/decrement the variable pointed to (which is assumed
   to have the given type), then pushes the value (r-value!) of the
   variable back on the stack.  The value pushed is that of the variable
   either before or after the inc/dec, depending on which operator
   was used.

   The size parameter is ignored unless type is TYPTR, in which case
   size should be the size (in bytes) of a datum pointed to by a
   pointer of this type. */


void b_inc_dec (TYPETAG type, B_INC_DEC_OP idop, unsigned int size)
{
  char *op;
  int reg_no;

  switch (idop) {
  case B_PRE_INC:
  case B_POST_INC:
  case B_PRE_DEC:
  case B_POST_DEC:
      break;
  default:
      bug("unrecognized idop in b_inc_dec");
  }

  emitn ("\t\t\t\t# b_inc_dec (");
  my_print_typetag (type);
  emit (", %s)", idop == B_PRE_INC ? "PRE-INC" : 
	               idop == B_POST_INC ? "POS-INC" :
	               idop == B_PRE_DEC ? "PRE-DEC" :
	                                    "POST-DEC");

  if (asm_section != SEC_TEXT)
    bug("non-text assembler section in b_inc_dec");

  if ((idop == B_PRE_INC) || (idop == B_POST_INC))
      op = "add";
  else
      op = "sub";

  if ((idop == B_POST_INC) || (idop == B_POST_DEC))
      reg_no = 2;
  else
      reg_no = 4;

  emit ("	ld	[%%sp], %%i0");  /* load the pointer (l-value) */

  switch (type) {

  case TYSIGNEDCHAR:                     
  case TYUNSIGNEDCHAR:                   
      emit ("	ldsb	[%%i0], %%i2");
      emit ("	%s	%%i2, 1, %%i4", op);
      emit ("	stb	%%i4, [%%i0]");
      emit ("	stb	%%i%d, [%%sp]", reg_no);
      break;

  case TYSIGNEDINT:                      
  case TYUNSIGNEDINT:                    
  case TYSIGNEDLONGINT:
  case TYUNSIGNEDLONGINT:
      emit ("	ld	[%%i0], %%i2");
      emit ("	%s	%%i2, 1, %%i4", op);
      emit ("	st	%%i4, [%%i0]");
      emit ("	st	%%i%d, [%%sp]", reg_no);
      break;

  case TYPTR:                            
      emit ("	ld	[%%i0], %%i2");
      emit ("	set	%u, %%i3", size);
      emit ("	%s	%%i2, %%i3, %%i4", op);
      emit ("	st	%%i4, [%%i0]");
      emit ("	st	%%i%d, [%%sp]", reg_no);
      break;
    
  case TYFLOAT:
      emit ("	ld	[%%i0], %%f2");
      if (!float_one_allocated) {
	  emit ("	.section	\".rodata\"");
	  emit ("	.align	4");
	  b_label (".Lf1");
	  b_alloc_float (1.0);
	  emit ("	.section	\".text\"");
	  float_one_allocated = TRUE;
      }
      emit ("	set	.Lf1, %%i1");
      emit ("	ld	[%%i1], %%f6");
      emit ("	f%ss	%%f2, %%f6, %%f4", op);
      emit ("	st	%%f4, [%%i0]");
      emit ("	st	%%f%d, [%%sp]", reg_no);
      break;
    
  case TYDOUBLE:
      emit ("	ldd	[%%i0], %%f2");
      if (!double_one_allocated) {
	  emit ("	.section	\".rodata\"");
	  emit ("	.align	8");
	  b_label (".Ld1");
	  b_alloc_double (1.0);
	  emit ("	.section	\".text\"");
	  double_one_allocated = TRUE;
      }
      emit ("	set	.Ld1, %%i1");
      emit ("	ldd	[%%i1], %%f6");
      emit ("	f%sd	%%f2, %%f6, %%f4", op);
      emit ("	std	%%f4, [%%i0]");
      emit ("	std	%%f%d, [%%sp]", reg_no);
      break;
    
  default:
    bug ("unsupported type in b_inc_dec");
  }
}






/* b_arith_rel_op accepts an arithmetic or relational operator
   and a type.  The operators are:

        B_ADD       add (+)
	B_SUB       substract (-) 
	B_MULT      multiply (*)
	B_DIV       divide (/)
	B_MOD       mod (%)
	B_LT        less than (<)
	B_LE        less than or equal to (<=)
	B_GT        greater than (>)
	B_GE        greater than or equal to (>=)
	B_EQ        equal (==)
	B_NE        not equal (!=)
   
   It assumes that two values of the indicated type are on the 
   stack.  It pops those values off the stack, performs the 
   indicated operation, and pushes the resulting value onto
   the stack.

   No arithmetic on pointers is allowed in this function,
   although pointer comparisons are okay.  For pointer arithmetic,
   use b_ptr_arith_op.

   NOTE:  For arithmetic operators that are not commutative, it
          assumes that the operands were pushed onto the stack
	  in left-to-right order (e.g. if the expression is
	  x - y, y is at the top of the stack and x is the 
	  next item below it.

   NOTE:  For relational operators, a value of either 1 (true)
          or 0 (false) is pushed onto the stack.         */


void b_arith_rel_op (B_ARITH_REL_OP arop, TYPETAG type)
{
  char *label;
  /* This flags signals that a reversal of arguments is needed. */
  int rev_flag=0;

  emitn ("\t\t\t\t# b_arith_rel_op (%s, ", b_arith_rel_op_string (arop));
  my_print_typetag (type);
  emit (")");

  switch (type) {
  case TYSIGNEDINT:
  case TYSIGNEDLONGINT:

      emit ("	ld	[%%sp], %%o1");
      b_pop();
      emit ("	ld	[%%sp], %%o0");

      switch (arop) {
      case B_ADD:
	  emit ("	add	%%o0, %%o1, %%o0");
	  break;
      case B_SUB:
	  emit ("	sub	%%o0, %%o1, %%o0");
	  break;
      case B_MULT:
	  emit ("	smul	%%o0, %%o1, %%o0");
	  break;
      case B_DIV:
	  emit ("	sdiv	%%o0, %%o1, %%o0");
	  break;
      case B_MOD:
	  emit ("	sub	%%sp, 96, %%sp");
	  emit ("	call	.rem, 0");
	  emit ("	nop");
	  emit ("	add	%%sp, 96, %%sp");
	  break;
      case B_LT:
      case B_LE:
      case B_GT:
      case B_GE:
	  emit ("	mov	1, %%o2");
	  emit ("	cmp	%%o0, %%o1");
	  emit ("	b%s	%s",
		arop==B_LT ? "l" :
		arop==B_LE ? "le" :
		arop==B_GT ? "g" : "ge",
		label = new_symbol());
	  emit ("	nop");
	  emit ("	mov	0, %%o2");
	  b_label(label);
	  emit ("	mov	%%o2, %%o0");
	  break;
      case B_EQ:
      case B_NE:
	  emit ("	xor	%%o0, %%o1, %%o0");
	  emit ("	subcc	%%g0, %%o0, %%g0");
	  emit ("	%sx	%%g0, %d, %%o0",
		arop==B_EQ ? "sub" : "add",
		-(arop==B_EQ));
	  break;
      default:
	  bug("unsupported op or op incompatible with type in b_arith_rel_op");
      }

      emit ("	st	%%o0, [%%sp]");
      break;

  case TYUNSIGNEDINT:
  case TYUNSIGNEDLONGINT:

      emit ("	ld	[%%sp], %%o1");
      b_pop();
      emit ("	ld	[%%sp], %%o0");

      switch (arop) {
      case B_ADD:
	  emit ("	add	%%o0, %%o1, %%o0");
	  break;
      case B_SUB:
	  emit ("	sub	%%o0, %%o1, %%o0");
	  break;
      case B_MULT:
	  emit ("	umul	%%o0, %%o1, %%o0");
	  break;
      case B_DIV:
	  emit ("	udiv	%%o0, %%o1, %%o0");
	  break;
      case B_MOD:
	  emit ("	call	.urem, 0");
	  emit ("	nop");
	  break;
      case B_LT:
      case B_GT:
	  emit ("	cmp	%%o%d, %%o%d", arop==B_GT, arop==B_LT);
	  emit ("	addx	%%g0, 0, %%o0");
	  break;
      case B_LE:
      case B_GE:
	  emit ("	cmp	%%o%d, %%o%d", arop==B_LE, arop==B_GE);
	  emit ("	subx	%%g0, -1, %%o0");
	  break;
      case B_EQ:
      case B_NE:
	  emit ("	xor	%%o0, %%o1, %%o0");
	  emit ("	subcc	%%g0, %%o0, %%g0");
	  emit ("	%sx	%%g0, %d, %%o0",
		arop==B_EQ ? "sub" : "add",
		-(arop==B_EQ));
	  break;
      default:
	  bug("unsupported op or op incompatible with type in b_arith_rel_op");
      }

      emit ("	st	%%o0, [%%sp]");
      break;

  case TYPTR:

      emit ("	ld	[%%sp], %%o1");
      b_pop();
      emit ("	ld	[%%sp], %%o0");

      switch (arop) {
      case B_LT:
      case B_GT:
	  emit ("	cmp	%%o%d, %%o%d", arop==B_GT, arop==B_LT);
	  emit ("	addx	%%g0, 0, %%o0");
	  break;
      case B_LE:
      case B_GE:
	  emit ("	cmp	%%o%d, %%o%d", arop==B_LE, arop==B_GE);
	  emit ("	subx	%%g0, -1, %%o0");
	  break;
      case B_EQ:
      case B_NE:
	  emit ("	xor	%%o0, %%o1, %%o0");
	  emit ("	subcc	%%g0, %%o0, %%g0");
	  emit ("	%sx	%%g0, %d, %%o0",
		arop==B_EQ ? "sub" : "add",
		-(arop==B_EQ));
	  break;
      default:
	  bug("unsupported op or op incompatible with type in b_arith_rel_op");
      }

      emit ("	st	%%o0, [%%sp]");
      break;

  case TYDOUBLE:

      emit ("	ldd	[%%sp], %%f4");
      b_pop();
      emit ("	ldd	[%%sp], %%f2");

      switch (arop) {
      case B_ADD:
      case B_SUB:
      case B_MULT:
      case B_DIV:
	  emit ("	f%sd	%%f2, %%f4, %%f2",
		arop==B_ADD ? "add" :
		arop==B_SUB ? "sub" :
		arop==B_MULT ? "mul" : "div");
	  emit ("	std	%%f2, [%%sp]");
	  break;
      case B_LT:
      case B_LE:
      case B_GT:
      case B_GE:
      case B_EQ:
      case B_NE:
	  emit ("	mov	1, %%o2");
	  emit ("	fcmp%sd	%%f2, %%f4", (arop==B_EQ||arop==B_NE)?"":"e");
	  emit ("	nop");
	  emit ("	fb%s	%s",
		arop==B_LT ? "l" :
		arop==B_LE ? "le" :
		arop==B_GT ? "g" :
		arop==B_GE ? "ge" :
		arop==B_EQ ? "e" : "ne",
		label = new_symbol());
	  emit ("	nop");
	  emit ("	mov	0, %%o2");
	  b_label(label);
	  emit ("	st	%%o2, [%%sp]");
	  break;
      default:
	  bug("unsupported op or op incompatible with type in b_arith_rel_op");
      }
      break;

  default:
      bug("unsupported type in b_arith_rel_op");
  }
}




/* b_ptr_arith_op takes an operator (which must be either B_ADD or B_SUB),
   the type of the second argument, and the size of object pointed to
   by the pointer argument(s).  It assumes that two values are on the
   stack: a pointer value (to an object of size size) as first argument,
   and either a pointer or int as second argument (specified by type).
   The second argument is on top of the stack.  b_ptr_arith_op pops the
   two arguments off the stack, performs the given pointer arithmetic
   operation, and pushes the result back on the stack.

   Only legal operations are performed, e.g., adding two pointers is not
   allowed.  The resulting value is a pointer, unless two pointers are
   subtracted, in which case the result is an integer.

   Note: this function does not handle pointer comparisons.  That is
   done in b_arith_rel_op. */


void b_ptr_arith_op (B_ARITH_REL_OP arop, TYPETAG type, unsigned int size)
{
  emitn ("\t\t\t\t# b_ptr_arith_op (%s, ", b_arith_rel_op_string (arop));
  my_print_typetag (type);
  emit (", size = %u)", size);

  if (size == 0)
      bug("size not positive in b_ptr_arith_op");

  emit ("	ld	[%%sp], %%o1");
  b_pop();
  emit ("	ld	[%%sp], %%o0");
  
  switch (type) {
  case TYSIGNEDINT:
  case TYUNSIGNEDINT:
  case TYSIGNEDLONGINT:
  case TYUNSIGNEDLONGINT:
      emit ("	set	%u, %%o2", size);
      emit ("	smul	%%o1, %%o2, %%o1");
      break;
  case TYPTR:
      break;
  default:
      bug("illegal type of second operand in b_ptr_arith_op");
  }

  switch (arop) {
  case B_ADD:
      if (type == TYPTR)
	  bug("cannot add pointer types in b_ptr_arith_op");
      emit ("	add	%%o0, %%o1, %%o0");
      break;
  case B_SUB:
      emit ("	sub	%%o0, %%o1, %%o0");
      if (type == TYPTR)
	  divide_by_size(size);
      break;
  default:
      bug("unsupported pointer operation in b_ptr_arith_op");
  }

  emit ("	st	%%o0, [%%sp]");
}





/* b_func_prologue accepts a function name and generates the prologue
   for a function with that name.  It also initializes four static
   variables that are used in b_store_formal_param. */


void b_func_prologue (char *f_name)
{
  emit ("\t\t\t\t# b_func_prologue (%s)", f_name);

  /* Args of type double will be stored starting at %fp-24. */
  loc_var_offset = double_base_offset = -24;
  /* The first 6 arg registers will be copied starting with this one. */
  formal_reg_no = 0;
  /* Stack locations for formal parameters start at %fp+68. */
  caller_offset = FUNC_LINK_OFFSET;

  if (asm_section != SEC_TEXT) {
      emit ("	.section	\".text\"");
      asm_section = SEC_TEXT;
  }
  emit ("	.align	4");
  emit ("	.global	%s", f_name);
  b_label (f_name);
  emit ("	save	%%sp, -24, %%sp");
}






/* b_init_formal_param_offset does the same thing as b_func_prologue,
   but only initializes the offset variables and emits no assembly code. */


void b_init_formal_param_offset ()
{
  emit ("\t\t\t\t# b_init_formal_param_offset ()");

  /* Args of type double will be stored starting at %fp-24. */
  loc_var_offset = double_base_offset = -24;
  /* The first 6 arg registers will be copied starting with this one. */
  formal_reg_no = 0;
  /* Stack locations for formal parameters start at %fp+68. */
  caller_offset = FUNC_LINK_OFFSET;
}






/* b_store_formal_param accepts the type of a parameter.  It must be called
   for each formal parameter, immediately after b_func_prologue.  It determines
   what register (if any) that parameter must be in, and emits code to
   move that parameter from the register to the stack at the appropriate
   offset relative to the frame pointer.  base_offset and double_base_offset
   are static variables maintained by the backend.  The function returns the
   offset (from the frame pointer) at which this
   parameter should be stored.  (For the curious, base_offset and
   double_base_offset get initialized in b_func_prologue.)

   For example, if your function has 3 parameters (int, int, double)
   this function would be called as follows:

       offset = b_store_formal_param (TYSIGNEDINT);
       offset = b_store_formal_param (TYSIGNEDINT);
       offset = b_store_formal_param (TYDOUBLE);

   Note that a call to b_store_formal_param must be made for each formal
   parameter in left-to-right order on the parameter list.

   All necessary type conversions are performed on the argument values,
   so, for example, calling

       offset = b_store_formal_param (TYFLOAT);
       offset = b_store_formal_param (TYSIGNEDCHAR);

   will automatically convert the first argument from double to float,
   and the second argument from int to char. */


int b_store_formal_param (TYPETAG type)
{
    /* We'll copy the args into the caller's frame, because that's what
       gcc apparently does.  Except for the first 6 words, which are stored
       in input registers, the arguments are already there.  Values of
       type double may be misaligned by the calling convention, so we'll
       copy them to new locations in the callee's frame, effectively
       pushing them onto the control stack. */

    emitn ("\t\t\t\t# b_store_formal_param (");
    my_print_typetag (type);
    emit (")");

    if (formal_reg_no < 0)
	bug("illegal input register number in b_store_formal_param");

    switch (type) {
    case TYSIGNEDCHAR:
    case TYUNSIGNEDCHAR:
    case TYSIGNEDINT:
    case TYUNSIGNEDINT:
    case TYSIGNEDLONGINT:
    case TYUNSIGNEDLONGINT:
    case TYPTR:
	if (formal_reg_no < 6)
	    emit ("	st	%%i%d, [%%fp+%d]",
		  formal_reg_no++, caller_offset);
	caller_offset += 4;
	/* if char, take the low-order byte */
	return caller_offset -
	    ((type==TYSIGNEDCHAR || type==TYUNSIGNEDCHAR) ? 1 : 4);
    case TYFLOAT:
    case TYDOUBLE:
	if (formal_reg_no < 6)
	    emit ("	st	%%i%d, [%%fp+%d]",
		  formal_reg_no++, caller_offset);
	emit ("	ld	[%%fp+%d], %%f2", caller_offset);
	caller_offset += 4;
	if (formal_reg_no < 6)
	    emit ("	st	%%i%d, [%%fp+%d]",
		  formal_reg_no++, caller_offset);
	emit ("	ld	[%%fp+%d], %%f3", caller_offset);
	caller_offset += 4;
	if (type == TYFLOAT)
	    emit ("	fdtos	%%f2, %%f2");
	emit ("	sub	%%sp, 8, %%sp");
	emit ("	st%s	%%f2, [%%sp]", type==TYFLOAT ? "" : "d");
	loc_var_offset = double_base_offset -= 8;
	return double_base_offset;
    default:
	bug ("unknown type in b_store_formal_param");
    }

    return 0;	/* unreachable */
}







/* b_get_formal_param_offset does the same thing as b_store_formal_param
   except that it only updates the offset variables and returns the offset
   of the parameter, without generating any assembly code. */


int b_get_formal_param_offset (TYPETAG type)
{
    /* We'll copy the args into the caller's frame, because that's what
       gcc apparently does.  Except for the first 6 words, which are stored
       in input registers, the arguments are already there.  Values of
       type double may be misaligned by the calling convention, so we'll
       copy them to new locations in the callee's frame, effectively
       pushing them onto the control stack. */

    emitn ("\t\t\t\t# b_get_formal_param_offset (");
    my_print_typetag (type);
    emit (")");

    if (formal_reg_no < 0)
	bug("illegal input register number in b_get_formal_param_offset");

    switch (type) {
    case TYSIGNEDCHAR:
    case TYUNSIGNEDCHAR:
    case TYSIGNEDINT:
    case TYUNSIGNEDINT:
    case TYSIGNEDLONGINT:
    case TYUNSIGNEDLONGINT:
    case TYPTR:
	if (formal_reg_no < 6)
	    formal_reg_no++;
	caller_offset += 4;
	/* if char, take the low-order byte */
	return caller_offset -
	    ((type==TYSIGNEDCHAR || type==TYUNSIGNEDCHAR) ? 1 : 4);
    case TYFLOAT:
    case TYDOUBLE:
	if (formal_reg_no < 6)
	    formal_reg_no++;
	caller_offset += 4;
	if (formal_reg_no < 6)
	    formal_reg_no++;
	caller_offset += 4;
	loc_var_offset = double_base_offset -= 8;
	return double_base_offset;
    default:
	bug ("unknown type in b_get_formal_param_offset");
    }

    return 0;	/* unreachable */
}







/* b_alloc_return_value allocates on the stack space to hold the return
   value for the current function.  This is only required for Pascal
   functions, where the return value can be set/updated any number of
   times and must persist across proc/func calls.

   Allocates 8 bytes and sets the global return_value_offset to the
   allocated space.
*/

void b_alloc_return_value()
{
    emit ("\t\t\t\t# b_alloc_return_value ( )");

    return_value_offset = b_alloc_local_vars(8);
}







/* b_alloc_local_vars accepts an integer and emits code to increase the
   stack by that number of bytes, adjusted upward to maintain doubleword
   (8-byte) alignment of %sp.  This function should be used to allocate
   space for all the variables in the declaration section of a block all
   all at once.  The size passed to b_alloc_local_vars must be at least
   the amount of space taken up by the variables, as well as any padding
   necessary for alignment.  The offset (from %fp) of the variable with
   lowest address is returned.  */


int b_alloc_local_vars (int size)
{
  /* Actual stack space allocated for the new local variables */
  int new_space;

  emit ("\t\t\t\t# b_alloc_local_vars ( size = %d )", size);

  if (size < 0)
      bug("negative size given to b_alloc_local_vars");

  new_space = next_multiple(size, 8);
  loc_var_offset -= new_space;
  emit ("	set	%d, %%i0", new_space);
  emit ("	sub	%%sp, %%i0, %%sp");
  return loc_var_offset;
}




/* b_get_local_var_offset returns the current value of loc_var_offset.
   In Pascal, local variable offsets must be computed long before space
   for them is actually allocated, so this function can be called once
   after all formal parameter offsets have been computed (using
   b_get_formal_param_offset), but before local variables are declared.
   The offset returned by b_get_local_var() is always negative and
   eight-byte aligned, and marks the point below which local variables
   may be allocated.  For a non-void function, the first eight-bytes
   (with offset b_get_local_var_offset() - 8) should be reserved for the
   return value of the function.  b_get_local_var_offset is completely
   passive--it emits no assembly code and has no effect on state
   variables.  */


int b_get_local_var_offset()
{
    return loc_var_offset;
}




/* b_dealloc_local_vars accepts an integer and emits code to decrease the
   stack by that number of bytes.  The stack pointer is restored
   (if necessary) to doubleword (8-byte) alignment.  */


void b_dealloc_local_vars (int size)
{
    int old_space = next_multiple(size, 8);
    emit ("\t\t\t\t# b_dealloc_local_vars ( size = %d )", size);

    if (size < 0)
	bug("negative size given to b_dealloc_local_vars");

    loc_var_offset += old_space;
    emit ("	set	%d, %%i0", old_space);
    emit ("	add	%%sp, %%i0, %%sp");
}





/* This is the only backend routine that performs the actual return
   from a C or Pascal function.  It is called from b_encode_return to
   execute a return statement, and also from b_func_epilogue when control
   falls out of the bottom of a function.
*/
static void b_void_return (void)
{
    emit ("	nop");
    emit ("	ret");
    emit ("	restore");	/* restore caller's frame in delay slot */
}







/* b_func_epilogue accepts the name of a function and emits code
   for the epilogue of a function by that name.  */


void b_func_epilogue (char *f_name)
{
  emit ("\t\t\t\t# b_func_epilogue (%s)", f_name);

  /* Reset this to an illegal value */
  return_value_offset = 0;
  b_void_return ();
}






/* b_set_return copies the value currently on the stack into the space
   designated for the return value, which should not be TYVOID.  The space
   designated for the return value is given by %fp + return_value_offset.
   The return_type argument is the return type of the function.
   b_set_return assumes that the type of the value currently on the stack
   is the same as return_type, and this value is popped after it is copied.

   This is only needed for Pascal functions, which can assign and update
   a return value anywhere, any number of times.
*/


void b_set_return (TYPETAG return_type)
{
  char * op_suffix = "";

  emitn ("\t\t\t\t# b_set_return (");
  my_print_typetag (return_type);
  emit  (")");

  if (return_type == TYVOID)
      bug("b_set_return: void return type");

  if (return_value_offset >= 0)
      bug("b_set_return: no space allocated for return value");

  switch (return_type) {
  case TYSIGNEDCHAR:
  case TYUNSIGNEDCHAR:
      op_suffix = "sb";
      break;
  case TYSIGNEDINT:
  case TYUNSIGNEDINT:
  case TYSIGNEDLONGINT:
  case TYUNSIGNEDLONGINT:
  case TYPTR:
      break;
  case TYFLOAT:
      break;
  case TYDOUBLE:
      op_suffix = "d";
      break;
  default:
      bug("b_set_return: illegal return type");
  }

  emit ("	ld%s	[%%sp], %%i0", op_suffix);
  emit ("	st%s	%%i0, [%%fp-%d]", op_suffix, -return_value_offset);
  b_pop();
}






/* b_prepare_return prepares for a return from a Pascal function or Pascal
   procedure.  The type argument is the return type of the function
   (TYVOID for a procedure).  Does nothing if TYVOID; otherwise assumes the
   value to be returned is at %fp + return_value_offset, and that its type
   is the same as return_type.  It loads this return value into the
   proper return register.

   This function should be called once for each function body, right before
   calling b_func_epilogue(), which does the actual return.

   For some mysterious reason, gcc usually moves integer return values
   into %o0 first, then copies into %i0 (the conventional return reg).
   We'll do the same, just to be safe.
*/


void b_prepare_return (TYPETAG return_type)
{
  emitn ("\t\t\t\t# b_prepare_return (");
  my_print_typetag (return_type);
  emit  (")");

  if (return_type == TYVOID)
      return;

  if (return_value_offset >= 0)
      bug("b_prepare_return: no return value allocated");

  switch (return_type) {
  case TYSIGNEDCHAR:
  case TYUNSIGNEDCHAR:
  case TYSIGNEDINT:
  case TYUNSIGNEDINT:
  case TYSIGNEDLONGINT:
  case TYUNSIGNEDLONGINT:
  case TYPTR:
      emit ("	ld%s	[%%fp-%d], %%o0",
	    return_type==TYSIGNEDCHAR ? "sb" :
	    return_type==TYUNSIGNEDCHAR ? "ub" : "",
	    -return_value_offset);
      emit ("	mov	%%o0, %%i0");
      break;
  case TYFLOAT:
  case TYDOUBLE:
      emit ("	ld%s	[%%fp-%d], %%f0", return_type==TYFLOAT ? "" : "d",
	    -return_value_offset);
      break;
  case TYVOID:
      break;
  default:
      bug("b_prepare_return: illegal return type");
  }
}





/* b_encode_return encodes a return statement in a C function.  The type
   argument is the type of the return expression (after assignment
   conversion to the return type of the function) if there is one.  If
   there is no return expression, TYVOID should be passed as the
   argument.  Assumes the value to be returned is on top of the stack,
   and that its type is the same as return_type.

   For some mysterious reason, gcc usually moves integer return values
   into %o0 first, then copies into %i0 (the conventional return reg).
*/


void b_encode_return (TYPETAG return_type)
{
  emitn ("\t\t\t\t# b_encode_return (");
  my_print_typetag (return_type);
  emit  (")");

  switch (return_type) {
  case TYSIGNEDCHAR:
  case TYUNSIGNEDCHAR:
  case TYSIGNEDINT:
  case TYUNSIGNEDINT:
  case TYSIGNEDLONGINT:
  case TYUNSIGNEDLONGINT:
  case TYPTR:
      emit ("	ld%s	[%%sp], %%o0",
	    return_type==TYSIGNEDCHAR ? "sb" :
	    return_type==TYUNSIGNEDCHAR ? "ub" : "");
      emit ("	mov	%%o0, %%i0");
      break;
  case TYFLOAT:
  case TYDOUBLE:
      emit ("	ld%s	[%%sp], %%f0", return_type==TYFLOAT ? "" : "d");
      break;
  case TYVOID:
      break;	
  default:
      bug("b_encode_return: illegal return type");
  }

  b_void_return ();
}






/* b_alloc_arglist takes the total size (in bytes) of actual arguments in a
   function call, and allocates space on the stack for the actual argument
   values of the function call.  It also pushes and initializes
   actual_arg_word_count[aa_top] to 0, and actual_arg_space[aa_top]
   to the total space used by the arguments.  The former value is used to
   count the words used by the actual arguments to the function, in order
   to determine register storage of actual parameters.  (The value is used
   and updated in b_load_arg).  A word is 4 bytes.  The latter value is
   used to pop the stack when the function returns.  This is done in
   b_funcall_by_name or b_funcall_by_ptr.

   NOTE: you must call b_alloc_arglist for each function call, even if
   no actual arguments are passed.  Also, every call to b_alloc_arglist
   must be followed (as in matching parentheses) by a call to either
   b_funcall_by_name or b_funcall_by_ptr, with zero or more calls to
   b_load_arg in between.  */


void b_alloc_arglist (int total_size)
{
    int arg_space = total_size;

    if (total_size < 0)
	bug("negative size for actual argument list in b_alloc_arglist");

    /* Always allocate at least 24 bytes for the six input registers */
    if (arg_space < 24)
	arg_space = 24;

    /* actual space to allocate on the stack -- The base address of the
       arg space is 4-byte-aligned, but not 8-byte-aligned; hence the
       +4 to ensure 4 bytes of padding before the base address, keeping
       %sp aligned on an 8-byte boundary. */
    arg_space = next_multiple(arg_space + 4, 8);

    /* Only display total_size as this is more useful to the user */
    emit ("\t\t\t\t# b_alloc_arglist (%d bytes)", total_size);

  /* push and initialize a new actual argument word count */
  actual_arg_word_count[++aa_top] = 0;
  actual_arg_space[aa_top] = arg_space;

  /* allocate space for the arguments */
  emit ("	sub	%%sp, %d, %%sp", arg_space);
}






/* b_load_arg accepts the type of an argument in a function call,
   and assumes that a value of this type is on top of the stack.
   It determines where in the argument list to put the argument:
   whether the argument should be in a register or on the stack.

   If the argument should be in a register, it emits code to move 
   the value of that argument (assumed to be at the top of the
   stack and of the proper type) from the stack to the proper 
   register.

   If the argument should be in the stack portion of the argument list,
   it emits code to move the value of that argument (assumed to be at the
   top of the stack and of the proper type) from the stack to another
   location in the stack (which has already been allocated by
   b_alloc_arglist) that will be the proper offset from the new
   frame pointer when control is transferred to the function.

   The word-offset of the argument is initialized in b_alloc_arglist,
   and is increased with each call to b_load_arg.

   For example, if your function is named "foo" with return type float
   and 3 actual parameters (int, double, double) this function would be
   called as follows:

       b_alloc_arglist(20);	// 20 bytes total - higher values are ok

       [here, put code to push the value of arg1 onto the stack]

       b_load_arg (TYSIGNEDINT);

       [here, put code to push the value of arg2 onto the stack]

       b_load_arg (TYDOUBLE);

       [here, put code to push the value of arg3 onto the stack]

       b_load_arg (TYDOUBLE);

       b_funcall_by_name ("foo", TYFLOAT);

   WARNING: it is assumed that the argument list lies direcly underneath the
   current top value on the stack.  Therefore, the sequence above must be
   followed strictly: b_load_arg is called after each push of an argument
   value, before the next argument is pushed.

   Note also that b_load_arg does NOT leave the value of the argument on
   the stack, i.e., the value is popped.    */


void b_load_arg (TYPETAG type)
{
    int word_count = actual_arg_word_count[aa_top];

    emitn ("\t\t\t\t# b_load_arg (");
    my_print_typetag (type);
    emit (")");

    switch (type) {
    case TYDOUBLE:
	emit ("	ldd	[%%sp], %%f2");
	b_pop();
	emit ("	st	%%f2, [%%sp+%d]", 4*word_count+4); /* +4 for padding */
	if (word_count < 6)
	    emit ("	ld	[%%sp+%d], %%o%d", 4*word_count+4, word_count);
	word_count++;
	emit ("	st	%%f3, [%%sp+%d]", 4*word_count + 4);
	if (word_count < 6)
	    emit ("	ld	[%%sp+%d], %%o%d", 4*word_count+4, word_count);
	word_count++;
	break;
    case TYSIGNEDINT:
    case TYUNSIGNEDINT:
    case TYSIGNEDLONGINT:
    case TYUNSIGNEDLONGINT:
    case TYPTR:
	emit ("	ld	[%%sp], %%i0");
	b_pop();
	emit ("	st	%%i0, [%%sp+%d]", 4*word_count+4);
	if (word_count < 6)
	    emit ("	mov	%%i0, %%o%d", word_count);
	word_count++;
	break;
    default:
	bug ("unpromoted function argument in b_load_arg");
    }

  /* Update top of word count stack for next argument */
  actual_arg_word_count[aa_top] = word_count;
}






/* b_funcall_by_name accepts a function name and a
   return type for the function.  It emits code to jump to 
   that function, pop any space off the stack used for actual
   arguments upon returning from the function, and push the return
   value (if any) of the function onto the stack.  Uses
   actual_arg_space[aa_top] to find the amount of space taken
   the arguments, and pops this value off of the actual_arg_space
   stack.  If the return type is TYVOID, then nothing is pushed on the
   stack upon return.

   Both b_funcall_by_name and b_funcall_by_ptr should only be used in
   conjunction with the routines b_alloc_arglist and b_load_args.
   Each call to b_funcall_by_name or b_funcall_by_ptr must be preceded
   (as with matching parentheses) by a call to b_alloc_arglist, with
   zero or more calls to b_load_args in between.

   b_funcall_by_name and b_funcall_by_ptr differ in only one way: the
   former requires an explicit function name as argument, while the
   latter assumes the entry address of the function has been pushed
   onto the stack.  */


void b_funcall_by_name (char *f_name, TYPETAG return_type)
{
  int arg_space;

  /* Get actual space that was allocated on the stack for actual args.
     Pop this and the current word count at the same time; we no longer
     need the latter, since we're done loading actual arguments. */
  arg_space = actual_arg_space[aa_top--];

  emitn ("\t\t\t\t# b_funcall_by_name (%s, ", f_name);
  /* emitn ("\t\t\t\t#                             "); */
  my_print_typetag (return_type);
  emit  (")");

  /* First make room for register window (set by callee's save instruction),
     then call the function. */
  emit ("	sub	%%sp, 64, %%sp");
  emit ("	call	%s, 0", f_name);
  emit ("	nop");		/* nothing in delay slot */

  /* Upon return, remove register window and argument list built
     for the call */
  emit ("	add	%%sp, %d, %%sp", arg_space + 64);

  if (return_type != TYVOID)
    /* Make room to push the return value */
    b_push ();

  switch (return_type) {

  case TYSIGNEDCHAR:
  case TYUNSIGNEDCHAR:
      emit ("	stb	%%o0, [%%sp]");
      break;

  case TYSIGNEDINT:
  case TYUNSIGNEDINT:
  case TYSIGNEDLONGINT:
  case TYUNSIGNEDLONGINT:
  case TYPTR:
      emit ("	st	%%o0, [%%sp]");
      break;

  case TYFLOAT:
    emit ("	st	%%f0, [%%sp]");
    break;

  case TYDOUBLE:
    emit ("	std	%%f0, [%%sp]");
    break;

  case TYVOID:
    break;

  default:
    bug ("unsupported type in b_funcall_by_name");
  }                                        
}






/* b_funcall_by_ptr accepts the return type for a function, and when
   called, assumes that the entry address of the function is on top
   of the stack.  It emits code to pop the entry address and jump to 
   that function, then upon return, pop any space for actual arguments
   used by function, then finally push the return value (if any) of 
   the function onto the stack.  Uses actual_arg_space[aa_top] to find
   the total space used by the actual arguments of the call, and pops
   this value off of the actual_arg_space stack.  The entry address of
   the function gets popped in the process.  If the return type
   is TYVOID, then nothing is pushed on the stack upon return.

   Both b_funcall_by_name and b_funcall_by_ptr should only be used in
   conjunction with the routines b_alloc_arglist and b_load_args.
   Each call to b_funcall_by_name or b_funcall_by_ptr must be preceded
   (as with matching parentheses) by a call to b_alloc_arglist, with
   zero or more calls to b_load_args in between.

   b_funcall_by_name and b_funcall_by_ptr differ in only one way: the
   former requires an explicit function name as argument, while the
   latter assumes the entry address of the function has been pushed
   onto the stack.  */


void b_funcall_by_ptr (TYPETAG return_type)
{
  int arg_space;

  /* Get actual space that was allocated on the stack for actual args.
     Pop this and the current word count at the same time; we no longer
     need the latter, since we're done loading actual arguments. */
  arg_space = actual_arg_space[aa_top--];

  emitn ("\t\t\t\t# b_funcall_by_ptr (");
  my_print_typetag (return_type);
  emit  (")");

  emit ("	ld	[%%sp], %%i0");   /* load procedure value */
  b_pop ();                               /* pop from stack */

  /* Ready to call the function.  First make room for register window
     (set by callee's save instruction), then call the function. */
  emit ("	sub	%%sp, 64, %%sp");
  emit ("	call	%%i0, 0");
  emit ("	nop");

  /* Upon return, remove register window and argument list built for
     the call */
  emit ("	add	%%sp, %d, %%sp", arg_space + 64);

  if (return_type != TYVOID)
    /* Make room to push the return value */
    b_push ();

  switch (return_type) {

  case TYSIGNEDCHAR:
  case TYUNSIGNEDCHAR:
      emit ("	stb	%%o0, [%%sp]");
      break;

  case TYSIGNEDINT:
  case TYUNSIGNEDINT:
  case TYSIGNEDLONGINT:
  case TYUNSIGNEDLONGINT:
  case TYPTR:
      emit ("	st	%%o0, [%%sp]");
      break;

  case TYFLOAT:
    emit ("	st	%%f0, [%%sp]");
    break;

  case TYDOUBLE:
    emit ("	std	%%f0, [%%sp]");
    break;

  case TYVOID:
    break;

  default:
    bug ("unsupported type in b_funcall_by_name");
  }                                        
}






/* b_global_decl emits the pseudo-op .data if beginning a data
   section.  In any case, it emits the pseudo-op .global for a global variable
   and a label for that variable, as well as an .align to the appropriate
   alignment.  A typical simple variable declaration
   (e.g. an int) is accomplished by a call to b_globl_decl followed
   by a call to the appropriate b_alloc function (b_alloc_int in the
   case of an int).

   For example, to emit code for the global declaration ``int i=5;'',
   one might call

   b_global_decl("i", 4);
   b_alloc_int(5, 1);

   For another example, to emit code for the global declaration
   ``int a[10] = {3,4,5};'', one might call

   b_global_decl("a", 4);
   b_alloc_int(3, 1);
   b_alloc_int(4, 1);
   b_alloc_int(5, 1);
   b_alloc_int(0, 7);
   */


void b_global_decl (char *id, int alignment, unsigned int size)
{
  emit ("\t\t\t\t# b_global_decl (%s, alignment = %d, size = %u)", id, alignment, size);

  emit (".globl %s", id);
  if (asm_section != SEC_DATA) {
    emit ("\t.data");
    asm_section = SEC_DATA;
  }
  emit ("\t.align\t%d", alignment);
  emit ("\t.type\t%s, @object", id);
  emit ("\t.size\t%s, %u", id, size);
  b_label (id);
}




#if 0

/* Should no longer be needed, since undefined is treated as global anyway.
   -SF 2/13/96 */

/* b_global_func_decl emits the pseudo-op .data if beginning a data 
   section.  In any case, it emits the pseudo-op .globl followed
   by the function name supplied.  This is the code needed for a
   function declaration.  */


void b_global_func_decl (char *func_name)
{
  emit ("\t\t\t\t# b_global_func_decl (%s)", func_name);

  if (in_data) {
    emit ("	.data");
    in_data = TRUE;
  }
  emit ("	.globl	%s", func_name);
}

#endif





/* The following seven functions emit code to allocate space for
   characters, short integers, integers, long integers, pointers, floats,
   and doubles, respectively.  In all cases, init is a required 
   initialization for the variable.  b_global_decl should be called
   once beforehand for the variable name (see header comments for this
   function).

   To allocate a pointer (and initialize it to 0), pass "0" (the string
   constant) as the first argument to b_alloc_ptr().
*/


void b_alloc_char (int init)
{
  emit ("\t.byte\t%d", init);
}



void b_alloc_short (int init)
{
    emit ("\t.value\t%d", init);
}



void b_alloc_int (int init)
{
  emit ("\t.long\t%d", init);
}



void b_alloc_long (long init)
{
  emit ("\t.long\t%ld", init);
}



void b_alloc_ptr (char *init)
{
  emit ("\t.long\t%s", init);
}


void b_alloc_float (double init)
{
  global_float_val = (float)init;
  emit ("\t.long\t%d", *(int *)&global_float_val);
}


void b_alloc_double (double init)
{
  global_double_val = init;
  emit ("\t.long\t%d", ((int *)&global_double_val)[0]);
  emit ("\t.long\t%d", ((int *)&global_double_val)[1]);
}





/* b_skip() allocates a given number of bytes of space in static storage.
   These bytes are zeroed.  Use b_skip() after a truncated
   initialization list.  */

void b_skip(unsigned int amount)
{
  emit("\t.zero\t%u", amount);
}




/*  emit prints printf strings to outfp  */

				
void emit( char *format, ... )      
{
        va_list ap;  
	va_start (ap, format);
	vfprintf(outfp, format, ap);
	va_end (ap);
	putc ('\n', outfp);
}
     





/*  emitn prints printf strings to outfp with no end-of-line character  */ 

			
void emitn( char *format, ... )      
{
        va_list ap;  
	va_start (ap, format);
	vfprintf(outfp, format, ap);
	va_end (ap);
		
}






/* b_label emits a label */


void b_label (char *label)
{
  emit ("%s:", label);
}






/* new_symbol generates unique symbols that can be used as 
   local labels in the assembly code being emitted.  */


char *new_symbol ()
{
  static int n = 0;
  static char buf[20];
  char *ret;

  sprintf (buf, ".L%d", n++);
  ret = strdup (buf);
  if ( !ret )
      bug( "new_symbol: out of memory" );
  return ret;
}





static void divide_by_size(unsigned int size)
{
    int i;
    unsigned int cur, prev;

    if (size == 0)
	bug("b_ptr_arith_op: size must be positive");

    for (i = 0; !(size & 0x00000001); i++, size >>= 1)
	;

    if (i > 0)
	emit("	sra	%%o0, %d, %%o0", i);

    if (size == 1)
	return;

    /* size is odd and > 2 */
    cur = size;
    prev = 1;
    i = 0;
    while (i < 8*sizeof(unsigned int) && cur != 1) {
	/* cur == size^{2^i} (mod 2^{32}) */
	/* prev == size^{2^i-1} (mod 2^{32}) */
	cur *= cur;
	prev *= prev*size;
	i++;
    }

    /* At this point, prev should be the modular multiplicative inverse of
       size (mod 2^{32}). */
    if (i >= 32 || prev*size != 1)
	bug("divide_by_size: modular arithmetic error!");

    /* %o1 is free for using. */
    emit("	set	%u, %%o1", prev);
    emit("	umul	%%o0, %%o1, %%o0");
}




/* b_lineno_comment generates a comment in the assembly code, displaying
   the given number.  It should be called from scan.l to generate the number
   of the new line as soon as a '\n' is detected in the source file. */


void b_lineno_comment (int lineno)
{
  emit (" #%5d", lineno);
}





/* b_arith_rel_op_string accepts an arithmetic/relational
   operator of type B_ARITH_REL_OP and returns a string
   indicating the nature of the arithmetic or relational
   operator.  */


char *b_arith_rel_op_string (B_ARITH_REL_OP arop)
{
  switch (arop) {
    
  case B_ADD:
    return " + ";

  case B_SUB:
    return " - ";

  case B_MULT:
    return " * ";

  case B_DIV:
    return " / ";

  case B_MOD:
    return " % ";

  case B_LT:
    return " < ";

  case B_LE:
    return " <= ";

  case B_GT:
    return " > ";

  case B_GE:
    return " >= ";

  case B_EQ:
    return " == ";

  case B_NE:
    return " != ";

  default:
    return "NO SUCH OP";
  }
}






/* my_print_typetag is a version of ty_print_typetag (found
   in types.c) that sends its output to stdout instead of
   stderr.  */


void my_print_typetag (TYPETAG tag)
    {

	switch (tag)
	    {
		    case TYARRAY:
			emitn ("array");
			break;

		    case TYBITFIELD:
			emitn ("bitfield");
			break;

		    case TYPTR:
			emitn ("pointer");
			break;

		    case TYSTRUCT:
			emitn ("structure");
			break;

		    case TYUNION:
			emitn ("union");
			break;

		    case TYENUM:
			emitn ("enumeration");
			break;

		    case TYFUNC:
			emitn ("function");
			break;

		    case TYFLOAT:
			emitn ("float");
			break;

		    case TYDOUBLE:
			emitn ("double");
			break;

		    case TYLONGDOUBLE:
			emitn ("long double");
			break;

		    case TYUNSIGNEDINT:
			emitn ("unsigned int");
			break;

		    case TYUNSIGNEDCHAR:
			emitn ("unsigned char");
			break;

		    case TYUNSIGNEDSHORTINT:
			emitn ("unsigned short int");
			break;

		    case TYUNSIGNEDLONGINT:
			emitn ("unsigned long int");
			break;

		    case TYSIGNEDCHAR:
			emitn ("signed char");
			break;

		    case TYSIGNEDINT:
			emitn ("signed int");
			break;

		    case TYSIGNEDLONGINT:
			emitn ("signed long int");
			break;

		    case TYSIGNEDSHORTINT:
			emitn ("signed short int");
			break;

		    case TYVOID:
			 emitn ("void");
			 break;

		    case TYERROR:
			 emitn ("error");
			 break;

		    default:
			 bug("illegal tag in \"my_print_typetag\"");
		}

    }

