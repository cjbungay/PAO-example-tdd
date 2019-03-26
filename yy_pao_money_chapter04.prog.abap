*&---------------------------------------------------------------------*
*& The Money Example for Test-Driven Development in ABAP
*&    based on TDD by Example:  Part 1;  Chapter 4
*&---------------------------------------------------------------------*
PROGRAM yy_pao_money_chapter04.

""  $5 + 10 CHF = $10 if rate is 2:1
**  Make "amount" private
""  Money rounding?
""  Equal null
""  Equal object


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
        RETURNING VALUE(rv_result) TYPE abap_bool,
      amount
        RETURNING VALUE(rv_amount) TYPE i.

  PRIVATE SECTION.
    DATA:
      mv_amount TYPE i.

ENDCLASS.

CLASS lcl_dollar IMPLEMENTATION.

  METHOD constructor.
    mv_amount = iv_amount.
  ENDMETHOD.

  METHOD times.
    ro_product = NEW lcl_dollar( amount( ) * iv_multiple ).
  ENDMETHOD.

  METHOD equals.
    DATA(lo_dollar) = CAST lcl_dollar( iv_data_object ).
    rv_result = xsdbool( amount( ) = lo_dollar->amount( ) ).
  ENDMETHOD.

  METHOD amount.
    rv_amount = mv_amount.
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
    assert_equals( exp = abap_true
                   act = lo_five->times( 2 )->equals( NEW lcl_dollar( 10 ) ) ).
    assert_equals( exp = abap_true
                   act = lo_five->times( 3 )->equals( NEW lcl_dollar( 15 ) ) ).
  ENDMETHOD.

  METHOD equality.
    DATA(lo_test_double) = NEW lcl_dollar( 5 ).
    assert_equals( exp = abap_true
                   act = lo_test_double->equals( NEW lcl_dollar( 5 ) ) ).
    assert_equals( exp = abap_false
                   act = lo_test_double->equals( NEW lcl_dollar( 6 ) ) ).
  ENDMETHOD.

ENDCLASS.
