*"* use this source file for your ABAP unit test classes
"! <p class="shorttext synchronized" lang="en">local Testclass</p>
CLASS ltcl_dom_check DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    TYPES: tty_dom_check TYPE STANDARD TABLE OF zdom_fix_test WITH DEFAULT KEY.

    CLASS-DATA:
    "! table for test data
      t_dom_check_data TYPE tty_dom_check.

    DATA:
      "! Class Object for Tests
      mo_cut           TYPE REF TO zcl_dom_check.  "class under test

    CLASS-METHODS:
      class_Setup,
      class_Teardown,
      "! Preparing Test data
      preparing_test_data.
    METHODS:

      setup,
      teardown,

      "! <p class="shorttext synchronized" lang="en">check single value allowed</p>
      check_single_value_allowed FOR TESTING
        RAISING cx_static_check,

      "! <p class="shorttext synchronized" lang="en">check single value forbidden</p>
      check_single_value_forbidden FOR TESTING
        RAISING cx_static_check,

      "! <p class="shorttext synchronized" lang="en">check interval value allowed</p>
      check_interval_value_allowed FOR TESTING
        RAISING cx_static_check,

      "! <p class="shorttext synchronized" lang="en">check interval value_forbidden</p>
      check_interval_value_forbidden FOR TESTING
        RAISING cx_static_check,

      "! <p class="shorttext synchronized" lang="en">check checktable allowed</p>
      check_checktable_allowed FOR TESTING
        RAISING cx_static_check,

      "! <p class="shorttext synchronized" lang="en">check checktable forbidden</p>
      check_checktable_forbidden FOR TESTING
        RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_dom_check IMPLEMENTATION.

  METHOD class_Setup.
 " Preparing Test data



    Preparing_Test_data(  ).





  ENDMETHOD.

  METHOD setup.

    mo_cut = zcl_dom_check=>get_instance( ).

  ENDMETHOD.

  METHOD teardown.
    CLEAR mo_cut.
    FREE mo_cut.
  ENDMETHOD.

  METHOD class_Teardown.
    CLEAR t_dom_check_data.
  ENDMETHOD.

  METHOD check_single_value_allowed.

    READ TABLE t_dom_check_data ASSIGNING FIELD-SYMBOL(<dataset>) with key zcount = 01.

      "Assert
    cl_abap_unit_assert=>assert_equals(
      exp   = abap_false
      act   = mo_cut->check_fix_values_struc( <dataset> )
      msg   = 'Single value allowed'
      level = if_aunit_constants=>critical
      quit  = if_aunit_constants=>no ).


  ENDMETHOD.

  METHOD check_single_value_forbidden.

    READ TABLE t_dom_check_data ASSIGNING FIELD-SYMBOL(<dataset>) with key zcount = 02.

      "Assert
    cl_abap_unit_assert=>assert_equals(
      exp   = abap_false
      act   = mo_cut->check_fix_values_struc( <dataset> )
      msg   = 'Single value forbidden'
      level = if_aunit_constants=>critical
      quit  = if_aunit_constants=>no ).

  ENDMETHOD.


  METHOD check_interval_value_allowed.

    READ TABLE t_dom_check_data ASSIGNING FIELD-SYMBOL(<dataset>) with key zcount = 03.

      "Assert
    cl_abap_unit_assert=>assert_equals(
      exp   = abap_false
      act   = mo_cut->check_fix_values_struc( <dataset> )
      msg   = 'Single interval allowed'
      level = if_aunit_constants=>critical
      quit  = if_aunit_constants=>no ).

  ENDMETHOD.

  METHOD check_interval_value_forbidden.

    READ TABLE t_dom_check_data ASSIGNING FIELD-SYMBOL(<dataset>) with key zcount = 04.

      "Assert
    cl_abap_unit_assert=>assert_equals(
      exp   = abap_false
      act   = mo_cut->check_fix_values_struc( <dataset> )
      msg   = 'Single interval forbidden'
      level = if_aunit_constants=>critical
      quit  = if_aunit_constants=>no ).

  ENDMETHOD.

  METHOD check_checktable_allowed.

    READ TABLE t_dom_check_data ASSIGNING FIELD-SYMBOL(<dataset>) with key zcount = 05.

      "Assert
    cl_abap_unit_assert=>assert_equals(
      exp   = abap_false
      act   = mo_cut->check_fix_values_struc( <dataset> )
      msg   = 'Single chacktable allowed'
      level = if_aunit_constants=>critical
      quit  = if_aunit_constants=>no ).

  ENDMETHOD.

  METHOD check_checktable_forbidden.

    READ TABLE t_dom_check_data ASSIGNING FIELD-SYMBOL(<dataset>) with key zcount = 06.

      "Assert
    cl_abap_unit_assert=>assert_equals(
      exp   = abap_false
      act   = mo_cut->check_fix_values_struc( <dataset> )
      msg   = 'Single checktable forbidden'
      level = if_aunit_constants=>critical
      quit  = if_aunit_constants=>no ).

  ENDMETHOD.


  METHOD preparing_test_data.
*BREAK-POINT.
    FIELD-SYMBOLS <test_data> TYPE zdom_fix_test .

    CLEAR: t_dom_check_data.
*----------------------------------------------------------------------*
    "               << Preparing Test data >>

     " test data for single value allowed
    APPEND INITIAL LINE TO t_dom_check_data ASSIGNING <test_data>.
    <test_data>-mandt  = sy-mandt.
    <test_data>-zcount = 01.
    <test_data>-dom_fix_01 = 'A'.


     " test data for single value forbidden
    APPEND INITIAL LINE TO t_dom_check_data ASSIGNING <test_data>.
    <test_data>-mandt  = sy-mandt.
    <test_data>-zcount = 02.
    <test_data>-dom_fix_01 = 'B'.

         " test data for interval value allowed
    APPEND INITIAL LINE TO t_dom_check_data ASSIGNING <test_data>.
    <test_data>-mandt  = sy-mandt.
    <test_data>-zcount = 03.

         " test data for interval value forbidden
    APPEND INITIAL LINE TO t_dom_check_data ASSIGNING <test_data>.
    <test_data>-mandt  = sy-mandt.
    <test_data>-zcount = 04.

    " test data for check table allowed
    APPEND INITIAL LINE TO t_dom_check_data ASSIGNING <test_data>.
    <test_data>-mandt  = sy-mandt.
    <test_data>-zcount = 05.

    " test data for check table forbidden
    APPEND INITIAL LINE TO t_dom_check_data ASSIGNING <test_data>.
    <test_data>-mandt  = '999'.
    <test_data>-zcount = 06.

  ENDMETHOD.

ENDCLASS.
