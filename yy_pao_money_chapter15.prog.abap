*&---------------------------------------------------------------------*
*& The Money Example for Test-Driven Development in ABAP
*&    based on TDD by Example:  Part 1;  Chapter 15
*&---------------------------------------------------------------------*
PROGRAM yy_pao_money_chapter15.

**  $5 + 10 CHF = $10 if rate is 2:1
""  Return Money from $5 + $5
""  Sum->plus
""  Expression->times


*& Production Code
CLASS lcl_money DEFINITION DEFERRED.
CLASS lcl_bank DEFINITION DEFERRED.

INTERFACE lif_expression.
  METHODS:
    reduce
      IMPORTING io_bank          TYPE REF TO lcl_bank
                iv_to            TYPE currency
      RETURNING VALUE(ro_result) TYPE REF TO lcl_money,
    plus
      IMPORTING io_addend     TYPE REF TO lif_expression
      RETURNING VALUE(ro_sum) TYPE REF TO lif_expression.
ENDINTERFACE.

CLASS lcl_money DEFINITION.

  PUBLIC SECTION.
    INTERFACES:
      lif_expression.
    ALIASES:
      reduce FOR lif_expression~reduce,
      plus   FOR lif_expression~plus.
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
      times
        IMPORTING iv_multiple       TYPE i
        RETURNING VALUE(ro_product) TYPE REF TO lif_expression,
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

CLASS lcl_sum DEFINITION.

  PUBLIC SECTION.
    INTERFACES
      lif_expression.
    ALIASES:
      reduce FOR lif_expression~reduce,
      plus   FOR lif_expression~plus.
    METHODS:
      constructor
        IMPORTING io_augend TYPE REF TO lif_expression
                  io_addend TYPE REF TO lif_expression,
      augend
        RETURNING VALUE(ro_augend) TYPE REF TO lif_expression,
      addend
        RETURNING VALUE(ro_addend) TYPE REF TO lif_expression.

  PRIVATE SECTION.
    DATA:
      mo_augend TYPE REF TO lif_expression,
      mo_addend TYPE REF TO lif_expression.

ENDCLASS.

CLASS lcl_bank DEFINITION.

  PUBLIC SECTION.
    METHODS:
      reduce
        IMPORTING io_source        TYPE REF TO lif_expression
                  iv_to            TYPE currency
        RETURNING VALUE(ro_result) TYPE REF TO lcl_money,
      rate
        IMPORTING iv_from        TYPE currency
                  iv_to          TYPE currency
        RETURNING VALUE(rv_rate) TYPE i,
      add_rate
        IMPORTING iv_from TYPE currency
                  iv_to   TYPE currency
                  iv_rate TYPE i.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_pair,
        from TYPE currency,
        to   TYPE currency,
        rate TYPE i,
      END OF ts_pair,
      tt_pair TYPE HASHED TABLE OF ts_pair WITH UNIQUE KEY from to.
    DATA:
      mt_rates TYPE tt_pair.

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
    ro_sum = NEW lcl_sum( io_augend = me
                          io_addend = io_addend ).
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

  METHOD reduce.
    DATA(lv_rate) = io_bank->rate( iv_from = currency( )
                                   iv_to   = iv_to ).
    ro_result = NEW lcl_money( iv_amount   = amount( ) / lv_rate
                               iv_currency = iv_to ).
  ENDMETHOD.

  METHOD amount.
    rv_amount = mv_amount.
  ENDMETHOD.

  METHOD currency.
    rv_currency = mv_currency.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_sum IMPLEMENTATION.

  METHOD constructor.
    mo_augend = io_augend.
    mo_addend = io_addend.
  ENDMETHOD.

  METHOD reduce.
    DATA(lv_amount) = augend( )->reduce( io_bank = io_bank
                                         iv_to   = iv_to )->amount( )
                    + addend( )->reduce( io_bank = io_bank
                                         iv_to   = iv_to )->amount( ).
    ro_result = NEW lcl_money( iv_amount   = lv_amount
                               iv_currency = iv_to ).
  ENDMETHOD.

  METHOD plus.
    ro_sum = VALUE #( ).
  ENDMETHOD.

  METHOD augend.
    ro_augend = mo_augend.
  ENDMETHOD.

  METHOD addend.
    ro_addend = mo_addend.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_bank IMPLEMENTATION.

  METHOD reduce.
    ro_result = io_source->reduce( io_bank = me
                                   iv_to   = iv_to ).
  ENDMETHOD.

  METHOD rate.
    rv_rate = COND #( WHEN iv_from = iv_to
                      THEN 1
                      ELSE VALUE #( mt_rates[ from = iv_from
                                              to   = iv_to ]-rate
                                    DEFAULT 0 ) ).
  ENDMETHOD.

  METHOD add_rate.
    INSERT VALUE #( from = iv_from
                    to   = iv_to
                    rate = iv_rate ) INTO TABLE mt_rates.
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
      simple_addition FOR TESTING,
      plus_returns_sum FOR TESTING,
      reduce_sum FOR TESTING,
      reduce_money FOR TESTING,
      reduce_different_currency FOR TESTING,
      identity_rate FOR TESTING,
      mixed_addition FOR TESTING.
ENDCLASS.

CLASS ltc_example IMPLEMENTATION.

  METHOD multiplication.
    DATA(lo_five) = lcl_money=>dollar( 5 ).
    assert_equals( exp = abap_true
                   act = lcl_money=>dollar( 10 )->equals( lo_five->times( 2 ) ) ).
    assert_equals( exp = abap_true
                   act = lcl_money=>dollar( 15 )->equals( lo_five->times( 3 ) ) ).
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

  METHOD plus_returns_sum.
    DATA(lo_five) = lcl_money=>dollar( 5 ).
    DATA(lo_result) = lo_five->plus( lo_five ).
    DATA(lo_sum) = CAST lcl_sum( lo_result ).
    assert_equals( exp = lo_five
                   act = lo_sum->augend( ) ).
    assert_equals( exp = lo_five
                   act = lo_sum->addend( ) ).
  ENDMETHOD.

  METHOD reduce_sum.
    DATA(lo_sum) = NEW lcl_sum( io_addend = lcl_money=>dollar( 3 )
                                io_augend = lcl_money=>dollar( 4 ) ).
    DATA(lo_bank) = NEW lcl_bank( ).
    DATA(lo_result) = lo_bank->reduce( io_source = lo_sum
                                       iv_to     = |USD| ).
    assert_equals( exp = abap_true
                   act = lo_result->equals( lcl_money=>dollar( 7 ) ) ).
  ENDMETHOD.

  METHOD reduce_money.
    DATA(lo_bank) = NEW lcl_bank( ).
    DATA(lo_result) = lo_bank->reduce( io_source = lcl_money=>dollar( 1 )
                                       iv_to     = |USD| ).
    assert_equals( exp = abap_true
                   act = lo_result->equals( lcl_money=>dollar( 1 ) ) ).
  ENDMETHOD.

  METHOD reduce_different_currency.
    DATA(lo_bank) = NEW lcl_bank( ).
    lo_bank->add_rate( iv_from = |CHF|
                       iv_to   = |USD|
                       iv_rate = 2 ).
    DATA(lo_result) = lo_bank->reduce( io_source = lcl_money=>franc( 2 )
                                       iv_to     = |USD| ).
    assert_equals( exp = abap_true
                   act = lo_result->equals( lcl_money=>dollar( 1 ) ) ).
  ENDMETHOD.

  METHOD identity_rate.
    DATA(lo_bank) = NEW lcl_bank( ).
    assert_equals( exp = 1
                   act = lo_bank->rate( iv_from = |USD|
                                        iv_to   = |USD| ) ).
  ENDMETHOD.

  METHOD mixed_addition.
    DATA(lo_five_bucks) = CAST lif_expression( lcl_money=>dollar( 5 ) ).
    DATA(lo_ten_francs) = CAST lif_expression( lcl_money=>franc( 10 ) ).
    DATA(lo_bank) = NEW lcl_bank( ).
    lo_bank->add_rate( iv_from = |CHF|
                       iv_to   = |USD|
                       iv_rate = 2 ).
    DATA(lo_result) = lo_bank->reduce( io_source = lo_five_bucks->plus( lo_ten_francs )
                                       iv_to     = |USD| ).
    assert_equals( exp = abap_true
                   act = lo_result->equals( lcl_money=>dollar( 10 ) ) ).
  ENDMETHOD.

ENDCLASS.
