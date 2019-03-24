*&---------------------------------------------------------------------*
*& The Money Example for Test-Driven Development in ABAP
*&    based on TDD by Example:  Part 1;  Chapter 6
*&---------------------------------------------------------------------*
PROGRAM yy_pao_money_chapter06.

""  $5 + 10 CHF = $10 if rate is 2:1
""  Money rounding?
""  Equal null
""  Equal object
""  Dollar/Franc duplication
**  Common equals
""  Common times


*& Production Code
CLASS lcl_money DEFINITION.

  PUBLIC SECTION.
    METHODS:
      equals
        IMPORTING iv_data_object   TYPE data
        RETURNING VALUE(rv_result) TYPE abap_bool,
      amount
        RETURNING VALUE(rv_amount) TYPE i.

  PROTECTED SECTION.
    DATA:
      mv_amount TYPE i.

ENDCLASS.

CLASS lcl_dollar DEFINITION
  INHERITING FROM lcl_money.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_amount TYPE i,
      times
        IMPORTING iv_multiple       TYPE i
        RETURNING VALUE(ro_product) TYPE REF TO lcl_dollar.

ENDCLASS.

CLASS lcl_franc DEFINITION
  INHERITING FROM lcl_money.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_amount TYPE i,
      times
        IMPORTING iv_multiple       TYPE i
        RETURNING VALUE(ro_product) TYPE REF TO lcl_franc.

ENDCLASS.

CLASS lcl_money IMPLEMENTATION.

  METHOD equals.
    DATA(lo_money) = CAST lcl_money( iv_data_object ).
    rv_result = xsdbool( amount( ) = lo_money->amount( ) ).
  ENDMETHOD.

  METHOD amount.
    rv_amount = mv_amount.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_dollar IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mv_amount = iv_amount.
  ENDMETHOD.

  METHOD times.
    ro_product = NEW lcl_dollar( amount( ) * iv_multiple ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_franc IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mv_amount = iv_amount.
  ENDMETHOD.

  METHOD times.
    ro_product = NEW lcl_franc( amount( ) * iv_multiple ).
  ENDMETHOD.

ENDCLASS.


*& Test Code
CLASS ltc_example DEFINITION
  INHERITING FROM cl_aunit_assert
  FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      multiplication FOR TESTING,
      equality FOR TESTING,
      franc_multiplication FOR TESTING.
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
    DATA(lo_test_dollar) = NEW lcl_dollar( 5 ).
    assert_equals( exp = abap_true
                   act = lo_test_dollar->equals( NEW lcl_dollar( 5 ) ) ).
    assert_equals( exp = abap_false
                   act = lo_test_dollar->equals( NEW lcl_dollar( 6 ) ) ).
    DATA(lo_test_franc) = NEW lcl_franc( 5 ).
    assert_equals( exp = abap_true
                   act = lo_test_franc->equals( NEW lcl_franc( 5 ) ) ).
    assert_equals( exp = abap_false
                   act = lo_test_franc->equals( NEW lcl_franc( 6 ) ) ).
  ENDMETHOD.

  METHOD franc_multiplication.
    DATA(lo_five) = NEW lcl_franc( 5 ).
    assert_equals( exp = abap_true
                   act = lo_five->times( 2 )->equals( NEW lcl_franc( 10 ) ) ).
    assert_equals( exp = abap_true
                   act = lo_five->times( 3 )->equals( NEW lcl_franc( 15 ) ) ).
  ENDMETHOD.

ENDCLASS.
