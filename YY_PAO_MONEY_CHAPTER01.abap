*&---------------------------------------------------------------------*
*& The Money Example for Test-Driven Development in ABAP
*&    based on TDD by Example:  Part 1;  Chapter 1
*&---------------------------------------------------------------------*
PROGRAM yy_pao_money_chapter01.

""  $5 + 10 CHF = $10 if rate is 2:1
**  #5 * 2 = $10
""  Make "amount" private
""  Dollar side-effects?
""  Money rounding?


*& Production Code
CLASS lcl_dollar DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_amount TYPE i,
      times
        IMPORTING iv_multiple TYPE i.
    DATA:
      mv_amount TYPE i.

ENDCLASS.

CLASS lcl_dollar IMPLEMENTATION.

  METHOD constructor.
    mv_amount = iv_amount.
  ENDMETHOD.

  METHOD times.
    mv_amount = mv_amount * iv_multiple.
  ENDMETHOD.

ENDCLASS.


*& Test Code
CLASS ltc_example DEFINITION
  INHERITING FROM cl_aunit_assert
  FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      multiplication FOR TESTING.
ENDCLASS.

CLASS ltc_example IMPLEMENTATION.

  METHOD multiplication.
    DATA(lo_five) = NEW lcl_dollar( 5 ).
    lo_five->times( 2 ).
    assert_equals( exp = 10
                   act = lo_five->mv_amount ).
  ENDMETHOD.

ENDCLASS.
