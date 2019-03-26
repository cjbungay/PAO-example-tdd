*&---------------------------------------------------------------------*
*& The Money Example for Test-Driven Development in ABAP
*&    based on TDD by Example:  Part 1;  Chapter 12
*&---------------------------------------------------------------------*
PROGRAM yy_pao_money_chapter12.

""  $5 + 10 CHF = $10 if rate is 2:1
**  $5 + $5 = $10


*& Production Code
INTERFACE lif_expression.

ENDINTERFACE.

CLASS lcl_money DEFINITION.

  PUBLIC SECTION.
    INTERFACES:
      lif_expression.
    CLASS-METHODS:
      dollar
        IMPORTING iv_amount        TYPE i
        RETURNING VALUE(ro_dollar) TYPE REF TO lcl_money,
      franc
        IMPORTING iv_amount       TYPE i
        RETURNING VALUE(ro_franc) TYPE REF TO lcl_money.
    METHODS:
      constructor
        IMPORTING iv_amount   TYPE i
                  iv_currency TYPE currency,
      plus
        IMPORTING io_addend     TYPE REF TO lcl_money
        RETURNING VALUE(ro_sum) TYPE REF TO lif_expression,
      times
        IMPORTING iv_multiple       TYPE i
        RETURNING VALUE(ro_product) TYPE REF TO lcl_money,
      equals
        IMPORTING iv_data_object   TYPE data
        RETURNING VALUE(rv_result) TYPE abap_bool,
      amount
        RETURNING VALUE(rv_amount) TYPE i,
      currency
        RETURNING VALUE(rv_currency) TYPE currency.

  PRIVATE SECTION.
    DATA:
      mv_amount   TYPE i,
      mv_currency TYPE currency.

ENDCLASS.

CLASS lcl_bank DEFINITION.

  PUBLIC SECTION.
    METHODS:
      reduce
        IMPORTING io_source        TYPE REF TO lif_expression
                  iv_to            TYPE currency
        RETURNING VALUE(ro_result) TYPE REF TO lcl_money.

ENDCLASS.

CLASS lcl_money IMPLEMENTATION.

  METHOD dollar.
    ro_dollar = NEW lcl_money( iv_amount   = iv_amount
                               iv_currency = |USD| ).
  ENDMETHOD.

  METHOD franc.
    ro_franc = NEW lcl_money( iv_amount   = iv_amount
                              iv_currency = |CHF| ).
  ENDMETHOD.

  METHOD constructor.
    mv_amount   = iv_amount.
    mv_currency = iv_currency.
  ENDMETHOD.

  METHOD plus.
    ro_sum = NEW lcl_money( iv_amount   = amount( ) + io_addend->amount( )
                            iv_currency = currency( ) ).
  ENDMETHOD.

  METHOD times.
    ro_product = NEW lcl_money( iv_amount   = amount( ) * iv_multiple
                                iv_currency = currency( ) ).
  ENDMETHOD.

  METHOD equals.
    DATA(lo_money) = CAST lcl_money( iv_data_object ).
    rv_result = xsdbool( amount( )   = lo_money->amount( ) AND
                         currency( ) = lo_money->currency( ) ).
  ENDMETHOD.

  METHOD amount.
    rv_amount = mv_amount.
  ENDMETHOD.

  METHOD currency.
    rv_currency = mv_currency.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_bank IMPLEMENTATION.

  METHOD reduce.
    ro_result = lcl_money=>dollar( 10 ).
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
      currency FOR TESTING,
      simple_addition FOR TESTING.
ENDCLASS.

CLASS ltc_example IMPLEMENTATION.

  METHOD multiplication.
    DATA(lo_five) = lcl_money=>dollar( 5 ).
    assert_equals( exp = abap_true
                   act = lo_five->times( 2 )->equals( lcl_money=>dollar( 10 ) ) ).
    assert_equals( exp = abap_true
                   act = lo_five->times( 3 )->equals( lcl_money=>dollar( 15 ) ) ).
  ENDMETHOD.

  METHOD equality.
    assert_equals( exp = abap_true
                   act = lcl_money=>dollar( 5 )->equals( lcl_money=>dollar( 5 ) ) ).
    assert_equals( exp = abap_false
                   act = lcl_money=>dollar( 5 )->equals( lcl_money=>dollar( 6 ) ) ).
    assert_equals( exp = abap_false
                   act = lcl_money=>franc( 5 )->equals( lcl_money=>dollar( 5 ) ) ).
  ENDMETHOD.

  METHOD currency.
    assert_equals( exp = |USD|
                   act = lcl_money=>dollar( 1 )->currency( ) ).
    assert_equals( exp = |CHF|
                   act = lcl_money=>franc( 1 )->currency( ) ).
  ENDMETHOD.

  METHOD simple_addition.
    DATA(lo_five) = lcl_money=>dollar( 5 ).
    DATA(lo_sum) = lo_five->plus( lo_five ).
    DATA(lo_bank) = NEW lcl_bank( ).
    DATA(lo_reduced) = lo_bank->reduce( io_source = lo_sum
                                        iv_to     = |USD| ).
    assert_equals( exp = abap_true
                   act = lo_reduced->equals( lcl_money=>dollar( 10 ) ) ).
  ENDMETHOD.

ENDCLASS.
