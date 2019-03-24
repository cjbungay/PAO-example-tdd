*&---------------------------------------------------------------------*
*& The Money Example for Test-Driven Development in ABAP
*&    based on TDD by Example:  Part 1;  Chapter 9
*&---------------------------------------------------------------------*
PROGRAM yy_pao_money_chapter09.

""  $5 + 10 CHF = $10 if rate is 2:1
""  Money rounding?
""  Equal null
""  Equal object
""  Dollar/Franc duplication
""  Common times
**  Currency?
""  Delete franc_multiplication FOR TESTING?


*& Production Code
CLASS lcl_money DEFINITION ABSTRACT.

  PUBLIC SECTION.
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
      times ABSTRACT
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
    METHODS:
      get_class
        RETURNING VALUE(rv_class) TYPE string.
    DATA:
      mv_amount   TYPE i,
      mv_currency TYPE currency.

ENDCLASS.

CLASS lcl_dollar DEFINITION
  INHERITING FROM lcl_money.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_amount TYPE i
                  iv_currency TYPE currency,
      times REDEFINITION.
ENDCLASS.

CLASS lcl_franc DEFINITION
  INHERITING FROM lcl_money.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_amount   TYPE i
                  iv_currency TYPE currency,
      times REDEFINITION.
ENDCLASS.

CLASS lcl_money IMPLEMENTATION.

  METHOD dollar.
    ro_dollar = NEW lcl_dollar( iv_amount   = iv_amount
                                iv_currency = |USD| ).
  ENDMETHOD.

  METHOD franc.
    ro_franc = NEW lcl_franc( iv_amount   = iv_amount
                              iv_currency = |CHF| ).
  ENDMETHOD.

  METHOD constructor.
    mv_amount   = iv_amount.
    mv_currency = iv_currency.
  ENDMETHOD.

  METHOD equals.
    DATA(lo_money) = CAST lcl_money( iv_data_object ).
    rv_result = xsdbool( amount( )    = lo_money->amount( ) AND
                         get_class( ) = lo_money->get_class( ) ).
  ENDMETHOD.

  METHOD get_class.
    rv_class = cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ).
  ENDMETHOD.

  METHOD amount.
    rv_amount = mv_amount.
  ENDMETHOD.

  METHOD currency.
    rv_currency = mv_currency.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_dollar IMPLEMENTATION.

  METHOD constructor.
    super->constructor( iv_amount   = iv_amount
                        iv_currency = iv_currency ).
  ENDMETHOD.

  METHOD times.
    ro_product = lcl_money=>dollar( amount( ) * iv_multiple ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_franc IMPLEMENTATION.

  METHOD constructor.
    super->constructor( iv_amount   = iv_amount
                        iv_currency = iv_currency ).
  ENDMETHOD.

  METHOD times.
    ro_product = lcl_money=>franc( amount( ) * iv_multiple ).
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
      franc_multiplication FOR TESTING.
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
    assert_equals( exp = abap_true
                   act = lcl_money=>franc( 5 )->equals( lcl_money=>franc( 5 ) ) ).
    assert_equals( exp = abap_false
                   act = lcl_money=>franc( 5 )->equals( lcl_money=>franc( 6 ) ) ).
    assert_equals( exp = abap_false
                   act = lcl_money=>franc( 5 )->equals( lcl_money=>dollar( 5 ) ) ).
  ENDMETHOD.

  METHOD currency.
    assert_equals( exp = |USD|
                   act = lcl_money=>dollar( 1 )->currency( ) ).
    assert_equals( exp = |CHF|
                   act = lcl_money=>franc( 1 )->currency( ) ).
  ENDMETHOD.

  METHOD franc_multiplication.
    DATA(lo_five) = lcl_money=>franc( 5 ).
    assert_equals( exp = abap_true
                   act = lo_five->times( 2 )->equals( lcl_money=>franc( 10 ) ) ).
    assert_equals( exp = abap_true
                   act = lo_five->times( 3 )->equals( lcl_money=>franc( 15 ) ) ).
  ENDMETHOD.

ENDCLASS.
