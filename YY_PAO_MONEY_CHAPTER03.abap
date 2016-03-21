*&---------------------------------------------------------------------*
*& The Money Example for Test-Driven Development in ABAP
*&    based on TDD by Example:  Part 1;  Chapter 3
*&---------------------------------------------------------------------*
PROGRAM yy_pao_money_chapter03.

""  $5 + 10 CHF = $10 if rate is 2:1
""  Make "amount" private
""  Money rounding?
**  equals( )


*& Production Code
CLASS lcl_dollar DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_amount TYPE i,
      times
        IMPORTING iv_multiple       TYPE i
        RETURNING VALUE(ro_product) TYPE REF TO lcl_dollar,
      equals
        IMPORTING iv_data_object   TYPE data
        RETURNING VALUE(rv_result) TYPE abap_bool.
    DATA:
      mv_amount TYPE i.

ENDCLASS.

CLASS lcl_dollar IMPLEMENTATION.

  METHOD constructor.
    mv_amount = iv_amount.
  ENDMETHOD.

  METHOD times.
    ro_product = NEW lcl_dollar( mv_amount * iv_multiple ).
  ENDMETHOD.

  METHOD equals.
    DATA(lo_dollar) = CAST lcl_dollar( iv_data_object ).
    rv_result = xsdbool( mv_amount = lo_dollar->mv_amount ).
  ENDMETHOD.

ENDCLASS.


*& Test Code
CLASS ltc_example DEFINITION
  INHERITING FROM cl_aunit_assert
  FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      multiplication FOR TESTING,
      equality FOR TESTING.
ENDCLASS.

CLASS ltc_example IMPLEMENTATION.

  METHOD multiplication.
    DATA(lo_five) = NEW lcl_dollar( 5 ).
    DATA(lo_product) = lo_five->times( 2 ).
    assert_equals( exp = 10
                   act = lo_product->mv_amount ).
    lo_product = lo_five->times( 3 ).
    assert_equals( exp = 15
                   act = lo_product->mv_amount ).
  ENDMETHOD.

  METHOD equality.
    DATA(lo_test_double) = NEW lcl_dollar( 5 ).
    assert_equals( exp = abap_true
                   act = lo_test_double->equals( NEW lcl_dollar( 5 ) ) ).
    assert_equals( exp = abap_false
                   act = lo_test_double->equals( NEW lcl_dollar( 6 ) ) ).
  ENDMETHOD.

ENDCLASS.
