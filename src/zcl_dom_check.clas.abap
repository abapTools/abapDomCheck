"! <p class="shorttext synchronized" lang="en">check value by domain value</p>
CLASS zcl_dom_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Class constructor</p>
    CLASS-METHODS class_constructor .

    "! <p class="shorttext synchronized" lang="en">Check data set against domain fixed values</p>
    "!
    "! @parameter im_dataset  | dataset for check
    "!
    "! @parameter rv_rejected       | Data set rejected = X
    METHODS check_fix_values_struc
      IMPORTING
        !im_dataset        TYPE any
      RETURNING
        VALUE(rv_rejected) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">get all messages</p>
    "! export the table of messages and clear messages in object
    "!
    "! @parameter r_result     | table of messages
    METHODS get_msg
      RETURNING
        VALUE(r_result) TYPE bal_tt_msg .

    "! <p class="shorttext synchronized" lang="en">get instance for this object</p>
    "!
    "! @parameter ro_object      | Returns instance object of zcl_dom_check
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_object) TYPE REF TO zcl_dom_check .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">fixed value of domain</p>
      BEGIN OF ty_fixvalue,
        tabname     TYPE ddobjname,
        fieldname   TYPE fieldname,
        rollname    TYPE rollname,
        domname     TYPE domname,
        position    TYPE tabfdpos,
        ddfixvalues TYPE ddfixvalues,
        checktable  TYPE tabname,
      END OF ty_fixvalue ,
      "! <p class="shorttext synchronized" lang="en">fixed values of domain</p>
      tty_fixvalues TYPE SORTED TABLE OF  ty_fixvalue WITH UNIQUE KEY tabname fieldname.
    METHODS msg_add_check
      IMPORTING
        im_fieldname  TYPE abap_compname
        im_fieldvalue TYPE any .


    "! <p class="shorttext synchronized" lang="en">Check data set against domain fixed values</p>
    "!
    "! @parameter  im_dataset       | Data set to be checked
    "!
    "! @parameter rv_rejected       | Data set rejected = X
    METHODS check_dom_fix_values
      IMPORTING
        im_dataset         TYPE any
      RETURNING
        VALUE(rv_rejected) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">check checktable</p>
    "!
    "! @parameter  im_dataset       | Data set to be checked
    "! @parameter  im_fixvalues     | fixed values of domain
    "!
    "! @parameter rv_rejected       | Data set rejected = X
    METHODS check_checktable_value
      IMPORTING
        im_dataset         TYPE data
        im_fixvalues       TYPE zcl_dom_check=>tty_fixvalues
      RETURNING
        VALUE(rv_rejected) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Single value check</p>
    "!
    "! @parameter  im_dataset       | Data set to be checked
    "! @parameter  im_fixvalues     | fixed values of domain
    "!
    "! @parameter rv_rejected       | Data set rejected = X
    METHODS check_single_value
      IMPORTING
        im_dataset         TYPE data
        im_fixvalues       TYPE zcl_dom_check=>tty_fixvalues
      RETURNING
        VALUE(rv_rejected) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Interval check</p>
    "!
    "! @parameter  im_dataset       | Data set to be checked
    "! @parameter  im_fixvalues     | fixed values of domain
    "!
    "! @parameter rv_rejected       | Data set rejected = X
    METHODS check_interval_value
      IMPORTING
        im_dataset         TYPE data
        im_fixvalues       TYPE zcl_dom_check=>tty_fixvalues
      RETURNING
        VALUE(rv_rejected) TYPE abap_bool.


    "! <p class="shorttext synchronized" lang="en">instance for this object</p>
    CLASS-DATA o_instance TYPE REF TO zcl_dom_check.

    DATA:
      "! <p class="shorttext synchronized" lang="en">Table of Messages</p>
      t_bal_msg   TYPE bal_tt_msg,
      "! <p class="shorttext synchronized" lang="en">fixed values of domain</p>
      t_fixvalues TYPE tty_fixvalues.

    "! <p class="shorttext synchronized" lang="en">Read domains Fixed values for a structure</p>
    "!
    "! @parameter  im_dataset       | Data set to be checked
    METHODS get_dom_fix_value
      IMPORTING
        im_dataset TYPE any.

    "! <p class="shorttext synchronized" lang="en">explode the structure</p>
    "!
    "! @parameter  im_tabname       | name of structure
    "!
    "! @parameter r_result          | fields of structure
    METHODS explode_structure
      IMPORTING
        im_tabname      TYPE ddobjname
      RETURNING
        VALUE(r_result) TYPE dfies_table.

    "! <p class="shorttext synchronized" lang="en">read fixed values of domain</p>
    "!
    "! @parameter  im_domname       | name of domain
    "!
    "! @parameter r_result          | fixed values of domain
    METHODS read_fixvalues
      IMPORTING
        im_domname      TYPE dfies-domname
      RETURNING
        VALUE(r_result) TYPE ddfixvalues.
    METHODS read_checktable
      IMPORTING
        im_domname      TYPE dfies-domname
      RETURNING
        VALUE(r_result) TYPE tabname.

ENDCLASS.



CLASS zcl_dom_check IMPLEMENTATION.


  METHOD check_checktable_value.

    DATA: lv_where       TYPE string,
          lt_tab_fields  TYPE ddfields,
          lo_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lo_elem TYPE REF TO cl_abap_elemdescr.

    lo_elem ?= cl_abap_elemdescr=>describe_by_data( im_dataset ).
    rv_rejected = abap_false.

    READ TABLE im_fixvalues ASSIGNING FIELD-SYMBOL(<fixvalue>) WITH KEY rollname = lo_elem->absolute_name+6.
    CHECK <fixvalue> IS ASSIGNED.
    CHECK NOT <fixvalue>-checktable IS INITIAL.
*----------------------------------------------------------------------*
    TRY.
        " Get the DDIC table details
        lo_structdescr ?= cl_abap_elemdescr=>describe_by_name( <fixvalue>-checktable ).
      CATCH cx_sy_move_cast_error.
        MESSAGE 'Fehler while casting'(s01) TYPE 'S'.
        RETURN.
    ENDTRY.


    " Is the table a DDIC table?
    CHECK lo_structdescr->is_ddic_type( ) = 'X'.
    " Get the DDIC table details
    lt_tab_fields = lo_structdescr->get_ddic_field_list( ).
*----------------------------------------------------------------------*
    " Finding the correct key field from the value table
    READ TABLE lt_tab_fields ASSIGNING FIELD-SYMBOL(<tab_key>)
                             WITH KEY domname = <fixvalue>-domname
                                      keyflag = 'X'.
    IF sy-subrc EQ 0. " Only continue if key field
      " Compose where clause
      CONCATENATE '''' im_dataset '''' INTO lv_where.
      CONCATENATE <tab_key>-fieldname '=' lv_where INTO lv_where SEPARATED BY space.

      " Cross-check the current value passed to the key field found.
      SELECT COUNT(*) FROM (<fixvalue>-checktable) WHERE (lv_where).
      IF sy-dbcnt EQ 0.
            rv_rejected = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD check_dom_fix_values.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
          lo_elem        TYPE REF TO cl_abap_elemdescr,
          tabname        TYPE ddobjname.
*----------------------------------------------------------------------*
    lo_structdescr ?= cl_abap_datadescr=>describe_by_data( im_dataset ).
    tabname = lo_structdescr->absolute_name+6.
    rv_rejected = abap_false.
*----------------------------------------------------------------------*
    DATA(lt_fixvalues) = FILTER tty_fixvalues( t_fixvalues "#EC CI_CONV_OK
                          WHERE tabname = tabname ).    "#EC CI_CONV_OK
*----------------------------------------------------------------------*

    LOOP AT lo_structdescr->components ASSIGNING FIELD-SYMBOL(<comp>).
      ASSIGN COMPONENT <comp>-name OF STRUCTURE im_dataset TO FIELD-SYMBOL(<component>).

      lo_elem ?= cl_abap_datadescr=>describe_by_data( <component> ).
      DATA(lv_fieldname) = lo_elem->absolute_name+6.


      DATA(rejected_single_value) = check_single_value( im_dataset   = <component>
                                                        im_fixvalues = lt_fixvalues ).

      DATA(rejected_interval_value) = check_interval_value( im_dataset   = <component>
                                                            im_fixvalues = lt_fixvalues ).

      DATA(rejected_checktable_value) = check_checktable_value( im_dataset   = <component>
                                                                im_fixvalues = lt_fixvalues ).

      IF   ( rejected_single_value      EQ abap_true )
        OR ( rejected_interval_value    EQ abap_true )
        OR ( rejected_checktable_value  EQ abap_true ).
        msg_add_check( im_fieldname  = <comp>-name
                 im_fieldvalue = im_dataset ).
        rv_rejected = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_fix_values_struc.
*----------------------------------------------------------------------*
    " Check the import parameters
    IF im_dataset IS INITIAL .

    ENDIF.
*----------------------------------------------------------------------*
    get_dom_fix_value( im_dataset ).

    rv_rejected = check_dom_fix_values( im_dataset ).

  ENDMETHOD.


  METHOD check_interval_value.

    DATA lo_elem TYPE REF TO cl_abap_elemdescr.

    lo_elem ?= cl_abap_elemdescr=>describe_by_data( im_dataset ).
    rv_rejected = abap_false.


    READ TABLE im_fixvalues ASSIGNING FIELD-SYMBOL(<fixvalue>) WITH KEY rollname = lo_elem->absolute_name+6.
    IF sy-subrc EQ 0.

    ENDIF.
  ENDMETHOD.


  METHOD check_single_value.

    DATA lo_elem TYPE REF TO cl_abap_elemdescr.

    lo_elem ?= cl_abap_elemdescr=>describe_by_data( im_dataset ).
    rv_rejected = abap_false.

    READ TABLE im_fixvalues ASSIGNING FIELD-SYMBOL(<fixvalue>) WITH KEY rollname = lo_elem->absolute_name+6.
    IF sy-subrc EQ 0.
      LOOP AT <fixvalue>-ddfixvalues ASSIGNING FIELD-SYMBOL(<ddfixvalue>) WHERE option = 'EQ'.

        IF <ddfixvalue>-low NE im_dataset.
          rv_rejected = abap_true.
        ELSE.
          rv_rejected = abap_false.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.
    o_instance = NEW #( ).
  ENDMETHOD.


  METHOD explode_structure.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = im_tabname
        all_types      = 'X'
      TABLES
        dfies_tab      = r_result
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  METHOD get_dom_fix_value.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
          ls_ddfixvalue  TYPE ty_fixvalue,
          lo_elem        TYPE REF TO cl_abap_elemdescr,
          lt_dfies_tab   TYPE dfies_table,
          tabname        TYPE ddobjname.
*----------------------------------------------------------------------*
    lo_structdescr ?= cl_abap_datadescr=>describe_by_data( im_dataset ).
    tabname = lo_structdescr->absolute_name+6.
*----------------------------------------------------------------------*
    " Check whether an entry for fixed values already exists.
    READ TABLE t_fixvalues WITH KEY tabname = tabname TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      RETURN.
    ENDIF.
*----------------------------------------------------------------------*
    lt_dfies_tab = explode_structure( tabname ).

    LOOP AT lt_dfies_tab ASSIGNING FIELD-SYMBOL(<dfies>).
      ls_ddfixvalue-tabname     = lo_structdescr->absolute_name+6.
      ls_ddfixvalue-fieldname   = <dfies>-fieldname.
      ls_ddfixvalue-rollname    = <dfies>-rollname.
      ls_ddfixvalue-domname     = <dfies>-domname.
      ls_ddfixvalue-position    = <dfies>-position.
*      ls_ddfixvalue-checktable  = <dfies>-checktable.
      ls_ddfixvalue-checktable  = read_checktable( <dfies>-domname ).
      ls_ddfixvalue-ddfixvalues = read_fixvalues( <dfies>-domname ).
      INSERT ls_ddfixvalue INTO TABLE t_fixvalues.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_instance.
    ro_object = o_instance.
  ENDMETHOD.


  METHOD get_msg.
    r_result = t_bal_msg.                               "#EC CI_CONV_OK
    CLEAR: t_bal_msg.
  ENDMETHOD.


  METHOD read_fixvalues.

    DATA(lt_fixvalues) = VALUE ddfixvalues(  ).
    DATA ls_ddfixvalue  TYPE ddfixvalue.
    DATA(lt_dd07v_tab)  = VALUE dd07v_tab( ).
    DATA(lv_rc)         = VALUE sy-subrc( ).

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = im_domname
        text           = abap_true
        langu          = sy-langu
      IMPORTING
        rc             = lv_rc
      TABLES
        dd07v_tab      = lt_dd07v_tab
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    LOOP AT lt_dd07v_tab ASSIGNING FIELD-SYMBOL(<dd07>).

*----------------------------------------------------------------------*
      CLEAR:   ls_ddfixvalue.
*************************************************************************
**                      single value                                   **
*************************************************************************
      IF (    ( <dd07>-domvalue_l NE space )
           OR ( <dd07>-valpos     NE space ) )
           AND ( <dd07>-domvalue_h EQ space ) .
        ls_ddfixvalue-low    = <dd07>-domvalue_l.
        ls_ddfixvalue-high   = <dd07>-domvalue_h.
        ls_ddfixvalue-option = 'EQ'.
        APPEND ls_ddfixvalue TO lt_fixvalues.
      ENDIF.
*************************************************************************
**                      interval value                                 **
*************************************************************************
      IF (    ( <dd07>-domvalue_h NE space )
          AND ( <dd07>-domvalue_l NE space )
        ).
        ls_ddfixvalue-low    = <dd07>-domvalue_l.
        ls_ddfixvalue-high   = <dd07>-domvalue_h.
        ls_ddfixvalue-option = 'BT'.
        APPEND ls_ddfixvalue TO lt_fixvalues.
      ENDIF.
    ENDLOOP.
*----------------------------------------------------------------------*

    IF NOT   ( lt_fixvalues IS INITIAL ) .
      r_result = lt_fixvalues.
    ENDIF.


  ENDMETHOD.

  METHOD read_checktable.

    SELECT SINGLE entitytab
        FROM dd01l
        INTO r_result
        WHERE domname EQ im_domname  .               "#EC CI_SEL_NESTED

  ENDMETHOD.

  METHOD msg_add_check.

    APPEND INITIAL LINE TO t_bal_msg ASSIGNING FIELD-SYMBOL(<msg>).

    <msg>-msgty    = 'W'.
    <msg>-msgid    = 'ZDOM_CHECK'.
    <msg>-msgno    = 001.
    <msg>-msgv1    = im_fieldvalue.
    <msg>-msgv2    = im_fieldname.

  ENDMETHOD.

ENDCLASS.
