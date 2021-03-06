*&---------------------------------------------------------------------*
*&  Include           ZPDYNMAS_NEW_CLS
*&---------------------------------------------------------------------*
CLASS lcl_general_view DEFINITION DEFERRED.
CLASS lcl_tree_view DEFINITION DEFERRED.

CLASS lcx_change_db_not_successfull DEFINITION INHERITING FROM cx_static_check FINAL.
ENDCLASS.

CLASS lcx_change_db_not_successfull IMPLEMENTATION.
ENDCLASS.

CLASS lcx_write_tr_not_successfull DEFINITION INHERITING FROM cx_static_check FINAL.
ENDCLASS.

CLASS lcx_write_tr_not_successfull IMPLEMENTATION.
ENDCLASS.

CLASS lcx_delete_not_successfull DEFINITION INHERITING FROM cx_static_check FINAL.
ENDCLASS.

CLASS lcx_delete_not_successfull IMPLEMENTATION.
ENDCLASS.

CLASS lcl_dynmas DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.

    TYPES: t_t588z TYPE TABLE OF t588z.

    CLASS-METHODS:
      "! returns instance of lcl_dynmas
      get_instance RETURNING VALUE(r_self) TYPE REF TO lcl_dynmas.


    METHODS:
      "! selects dynamic actions
      select_dynmas.

    METHODS:
      "! returns complete T588Z
      get_whole_table RETURNING VALUE(table) TYPE z_t_t588z.

    METHODS:
      "! returns every T588Z entry for the given infotype
      get_for_infty  IMPORTING infty                   TYPE infty
                     RETURNING VALUE(rt_all_for_infty) TYPE z_t_t588z,

      "! returns every T588Z entry for the given infotype and subtype
      get_for_subty  IMPORTING infty                   TYPE infty
                               subty                   TYPE subty
                     RETURNING VALUE(rt_all_for_subty) TYPE z_t_t588z,

      "! returns every T588Z entry for the given infotype, subtype and the fieldname
      get_for_field  IMPORTING infty                   TYPE infty
                               subty                   TYPE subty
                               field                   TYPE fieldname
                     RETURNING VALUE(rt_all_for_field) TYPE z_t_t588z,

      "! returns every T588Z entry for the given infotype, subtype, fieldname and the action
      get_for_actio IMPORTING infty                   TYPE infty
                              subty                   TYPE subty
                              field                   TYPE fieldname
                              actio                   TYPE opera
                    RETURNING VALUE(rt_all_for_actio) TYPE z_t_t588z.

    METHODS:
      "! Save changes into DB-Table T588Z and write an transport request
      save IMPORTING current_t588z TYPE z_t_t588z
                     origin_t588z  TYPE z_t_t588z.

  PRIVATE SECTION.


    METHODS: constructor.

    METHODS:
      "! write changes into transport request <br>
      "! @Exception: If no entry got written into an TR --> exception
      write_transport_request IMPORTING t588z                    TYPE z_t_t588z
                              RETURNING VALUE(transport_request) TYPE REF TO zif_trs_transport_request
                              RAISING   lcx_write_tr_not_successfull.

    METHODS:
      "! writing the changes into the database table <br>
      "! @Exception: If there was any error while writing into the db --> Exception
      change_db_table IMPORTING origin_t588z  TYPE z_t_t588z
                                current_t588z TYPE z_t_t588z
                      RAISING   lcx_change_db_not_successfull.

    METHODS:
      "! Adds shown lines into saving table.
      prepare_table_for_save IMPORTING current_shown_t588z  TYPE z_t_t588z
                                       original_shown_t588z TYPE z_t_t588z
                             RETURNING VALUE(result)        TYPE z_t_t588z.

    METHODS:
      "! Deletes the modified entries from table complete_t588z
      delete_modified_entries RAISING lcx_delete_not_successfull.

    CLASS-DATA:
      self TYPE REF TO lcl_dynmas.

    DATA:
      view_control TYPE REF TO lcl_general_view.

    DATA:
      complete_t588z TYPE z_t_t588z.

ENDCLASS.

CLASS lcl_table_view DEFINITION.

  PUBLIC SECTION.

    METHODS:
      pbo.

    METHODS:
      insert,
      delete,
      clear_flag,
      insert_flag.

    METHODS:
      get_origin_table RETURNING VALUE(origin) TYPE z_t_t588z.

    CLASS-METHODS:
      rebuild_numbers.

    CLASS-METHODS: show_table IMPORTING it_t588z TYPE z_t_t588z.

    CLASS-METHODS: has_table_changed RETURNING VALUE(changed) TYPE abap_bool.
    CLASS-METHODS: get_shown_table RETURNING VALUE(shown) TYPE z_t_t588z.

  PRIVATE SECTION.

    CLASS-DATA:
      origin_shown_table TYPE z_t_t588z.

    METHODS:
      loop_at_screen.

ENDCLASS.

CLASS lcl_tree_view DEFINITION.

  PUBLIC SECTION.

    TYPES:
      " Type for nodes
      BEGIN OF t_keytab,
        node_key TYPE num10,
        ntype    TYPE char01, " I=INFTY, S=SUBTY, F=FNAME, O=OPERA
        infty    TYPE infty,
        subty    TYPE subty,
        fname    TYPE fieldname,
        opera    TYPE opera,
      END OF t_keytab.

    CLASS-DATA:
      tree_container TYPE REF TO cl_gui_custom_container.

    METHODS:
      pbo.

    METHODS:
      add_node IMPORTING node_type  TYPE char01
                         t588z_line TYPE t588z.
    METHODS:
      get_nodes RETURNING VALUE(r_node_table) TYPE treev_ntab,
      get_items RETURNING VALUE(r_item_table) TYPE iwb_mtreeitm.

    METHODS:
      handle_node_double_click FOR EVENT node_double_click OF cl_gui_column_tree
        IMPORTING node_key,

      handle_item_double_click FOR EVENT item_double_click OF cl_gui_column_tree
        IMPORTING node_key item_name.

  PRIVATE SECTION.

    DATA:
      tree_grid        TYPE REF TO cl_gui_column_tree.

    " Tree Attributes
    DATA:
      node_counter   TYPE num10,
      node_table     TYPE treev_ntab,
      last_infty_key TYPE treev_node-node_key,
      last_subty_key TYPE treev_node-node_key,
      last_fname_key TYPE treev_node-node_key.

    DATA:
      item_table TYPE iwb_mtreeitm,
      keytab     TYPE TABLE OF t_keytab.


    METHODS:
      add_node_as_infty IMPORTING t588z_line TYPE t588z,
      add_node_as_subty IMPORTING t588z_line TYPE t588z,
      add_node_as_field IMPORTING t588z_line TYPE t588z,
      add_node_as_funct IMPORTING t588z_line TYPE t588z.

    METHODS:
      add_item IMPORTING node_key TYPE treev_node-node_key
                         txt      TYPE string,
      add_key IMPORTING t588z_line TYPE t588z
                        node_key   TYPE treev_node-node_key
                        ntype      TYPE char01.

ENDCLASS.

CLASS lcl_general_view DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS:
      get_instance RETURNING VALUE(r_self) TYPE REF TO lcl_general_view,
      pbo.

    METHODS:
      add_node IMPORTING node_type  TYPE char01
                         t588z_line TYPE t588z.

    METHODS:
      change_mode IMPORTING read_mode  TYPE abap_bool OPTIONAL
                            write_mode TYPE abap_bool OPTIONAL
                              PREFERRED PARAMETER read_mode,
      get_active_read_mode RETURNING VALUE(r_active_read_mode) TYPE abap_bool.

    METHODS:
      insert,
      delete,
      mark_all,
      unmark_all.

    METHODS:
      set_focus IMPORTING s_focus TYPE lcl_tree_view=>t_keytab,
      get_focus RETURNING VALUE(r_focus) TYPE lcl_tree_view=>t_keytab.

    METHODS:
      save_changes.

  PRIVATE SECTION.

    METHODS:
      constructor.

    DATA:
      active_read_mode TYPE abap_bool.

    DATA:
      focus TYPE lcl_tree_view=>t_keytab.

    CLASS-DATA:
      self TYPE REF TO lcl_general_view.

    DATA:
      tree_view  TYPE REF TO lcl_tree_view,
      table_view TYPE REF TO lcl_table_view.

    METHODS:
      init_tree_view,
      init_table_view.

ENDCLASS.

CLASS lcl_dynmas IMPLEMENTATION.

  METHOD get_instance.
    IF self IS NOT BOUND.
      self = NEW lcl_dynmas( ).
    ENDIF.
    r_self = self.
  ENDMETHOD.

  METHOD constructor.
    view_control = lcl_general_view=>get_instance( ).
  ENDMETHOD.

  METHOD select_dynmas.

    SELECT * FROM t588z INTO TABLE complete_t588z.

    LOOP AT complete_t588z INTO DATA(t588z).

      AT NEW infty.
        view_control->add_node( node_type = 'I' t588z_line = t588z ).
      ENDAT.

      AT NEW subty.
        IF t588z-subty IS INITIAL.
          t588z-subty = '****'.
        ENDIF.
        view_control->add_node( node_type = 'S' t588z_line = t588z ).
      ENDAT.

      AT NEW fname.
        IF t588z-fname IS INITIAL.
          t588z-fname = '*****'.
        ENDIF.
        view_control->add_node( node_type = 'F' t588z_line = t588z ).
      ENDAT.

      AT NEW opera.
        view_control->add_node( node_type = 'O' t588z_line = t588z ).
      ENDAT.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_whole_table.
    table = complete_t588z.
  ENDMETHOD.

  METHOD get_for_infty.
    rt_all_for_infty = FILTER z_t_t588z( complete_t588z WHERE mandt = sy-mandt AND infty = infty ).
  ENDMETHOD.

  METHOD get_for_subty.
    rt_all_for_subty = FILTER z_t_t588z( complete_t588z
                          WHERE mandt = sy-mandt
                            AND infty = infty
                            AND subty = subty ).
  ENDMETHOD.

  METHOD get_for_field.
    rt_all_for_field = FILTER z_t_t588z( complete_t588z
                          WHERE mandt = sy-mandt
                            AND infty = infty
                            AND subty = subty
                            AND fname = field ).
  ENDMETHOD.

  METHOD get_for_actio.
    rt_all_for_actio = FILTER z_t_t588z( complete_t588z
                          WHERE mandt = sy-mandt
                            AND infty = infty
                            AND subty = subty
                            AND fname = field
                            AND opera = actio ).
  ENDMETHOD.

  METHOD save.

    DATA(changed_shown_t588z) = me->prepare_table_for_save( current_shown_t588z  = current_t588z
                                                            original_shown_t588z = origin_t588z ).

    TRY.
        me->delete_modified_entries( ).
      CATCH lcx_delete_not_successfull.
        RETURN.
    ENDTRY.

    INSERT LINES OF current_t588z INTO complete_t588z INDEX sy-tabix.

    " change DB and write transport request
    TRY.
        me->change_db_table( origin_t588z  = origin_t588z
                             current_t588z = current_t588z ).

        me->write_transport_request( t588z = changed_shown_t588z ).
        COMMIT WORK.
      CATCH: lcx_change_db_not_successfull,
             lcx_write_tr_not_successfull.
        MESSAGE |{ 'Something went wrong'(msg) }| TYPE 'I'.
        ROLLBACK WORK.
        RETURN.
    ENDTRY.



  ENDMETHOD.

  METHOD write_transport_request.

    DATA:
      lt_e071  TYPE TABLE OF e071,
      lt_e071k TYPE TABLE OF e071k,
      ls_e071k TYPE e071k,
      tr_order TYPE trkorr,
      tr_task  TYPE trkorr.

    " Version 1.0
    CALL FUNCTION 'TRINT_ORDER_CHOICE'
      EXPORTING
        wi_order_type = 'W'
        wi_task_type  = 'Q'
        wi_category   = 'CUST'
      IMPORTING
        we_order      = tr_order
        we_task       = tr_task
      TABLES
        wt_e071       = lt_e071
        wt_e071k      = lt_e071k
      EXCEPTIONS
        OTHERS        = 6.

    IF sy-subrc = 0.

      DATA(ls_e071) = VALUE e071( as4pos = 0
                                  pgmid = 'R3TR'
                                  object = 'TABU'
                                  obj_name = 'T588Z'
                                  objfunc = 'K' ).
      APPEND ls_e071 TO lt_e071.

      LOOP AT t588z INTO DATA(s_t588z).

        CONCATENATE sy-mandt s_t588z-infty s_t588z-subty s_t588z-fname
                s_t588z-opera s_t588z-seqno INTO ls_e071k-tabkey.

        ls_e071k-pgmid = 'R3TR'.
        ls_e071k-as4pos = 0.
        ls_e071k-object = ls_e071k-mastertype = 'TABU'.
        ls_e071k-objname = ls_e071k-mastername = 'T588Z'.

        APPEND ls_e071k TO lt_e071k.
        CLEAR ls_e071k.

      ENDLOOP.

      IF lines( lt_e071k ) > 0 AND lines( lt_e071 ) > 0.
        CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
          EXPORTING
            wi_trkorr = tr_task
          TABLES
            wt_e071   = lt_e071
            wt_e071k  = lt_e071k
          EXCEPTIONS
            OTHERS    = 68.
        IF sy-subrc = 0.
          MESSAGE s000(vz) WITH |{ 'Entries are in Transport Request '(001) } { tr_order }|.
        ELSE.
          RAISE RESUMABLE EXCEPTION TYPE lcx_write_tr_not_successfull.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD change_db_table.

    DATA: vinfo      TYPE vargt,
          kennz      TYPE kenna,
          t588z_line TYPE t588z,
          highest_rc LIKE sy-subrc VALUE 0.


    LOOP AT origin_t588z INTO DATA(origin).

      CLEAR: t588z_line, kennz, vinfo.

      " check if the line does still exist --> modify KENNZ and VINFO in database
      IF line_exists( current_t588z[ infty = origin-infty subty = origin-subty fname = origin-fname
                                     opera = origin-opera seqno = origin-seqno ] ).

        t588z_line = current_t588z[ infty = origin-infty subty = origin-subty fname = origin-fname
                                     opera = origin-opera seqno = origin-seqno ].

        kennz = t588z_line-kennz.
        vinfo = t588z_line-vinfo.

        " if both lines are equal the update won't be performed and sy-subrc would be 4 --> exception
        IF origin = t588z_line.
          CONTINUE.
        ENDIF.

        UPDATE t588z SET kennz = kennz vinfo = vinfo
                   WHERE infty = t588z_line-infty
                     AND subty = t588z_line-subty
                     AND fname = t588z_line-fname
                     AND opera = t588z_line-opera
                     AND seqno = t588z_line.

        IF sy-subrc = 4.
          RAISE RESUMABLE EXCEPTION TYPE lcx_change_db_not_successfull.
        ENDIF.

      ELSE.
        " delete every entry which is currently not in the table
        DELETE FROM t588z WHERE infty = origin-infty AND subty = origin-subty
                            AND fname = origin-fname AND opera = origin-opera AND seqno = origin-seqno.
        IF sy-subrc <> 0.
          RAISE RESUMABLE EXCEPTION TYPE lcx_change_db_not_successfull.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CLEAR: origin, t588z_line, kennz, vinfo.

    " every line which was not in the origin table should get inserted into t588z
    LOOP AT current_t588z INTO DATA(current).

      " check if line existed in the origin table
      IF line_exists( origin_t588z[ infty = current-infty subty = current-subty fname = current-fname
                                    opera = current-opera seqno = current-seqno ] ).
        "if yes continue with next entry ( we already processed it )
        CONTINUE.
      ELSE.
        " if not insert it into t588z
        INSERT INTO t588z VALUES @current.

        IF sy-subrc <> 0.
          RAISE RESUMABLE EXCEPTION TYPE lcx_change_db_not_successfull.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD prepare_table_for_save.

    result = original_shown_t588z.

    LOOP AT current_shown_t588z INTO DATA(shown_t588z).

      IF NOT line_exists( original_shown_t588z[ infty = shown_t588z-infty
                                                subty = shown_t588z-subty
                                                fname = shown_t588z-fname
                                                opera = shown_t588z-opera
                                                seqno = shown_t588z-seqno ] ).
        INSERT shown_t588z INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD delete_modified_entries.

    DATA(current_focus) = lcl_general_view=>get_instance( )->get_focus( ).

    CASE current_focus-ntype.
      WHEN 'I'.
        READ TABLE complete_t588z INTO DATA(dummy)
        WITH KEY infty = current_focus-infty.

        DELETE complete_t588z WHERE infty = current_focus-infty.
      WHEN 'S'.
        READ TABLE complete_t588z INTO dummy
        WITH KEY infty = current_focus-infty subty = current_focus-subty.

        DELETE complete_t588z WHERE infty = current_focus-infty AND subty = current_focus-subty.
      WHEN 'F'.
        READ TABLE complete_t588z INTO dummy
        WITH KEY infty = current_focus-infty subty = current_focus-subty
                 fname = current_focus-fname.

        DELETE complete_t588z WHERE infty = current_focus-infty AND subty = current_focus-subty
                                AND fname = current_focus-fname.
      WHEN 'O'.
        READ TABLE complete_t588z INTO dummy
        WITH KEY infty = current_focus-infty subty = current_focus-subty
                 fname = current_focus-fname opera = current_focus-opera.

        DELETE complete_t588z WHERE infty = current_focus-infty AND subty = current_focus-subty
                                AND fname = current_focus-fname AND opera = current_focus-opera.
    ENDCASE.

    IF sy-subrc <> 0.
      RAISE RESUMABLE EXCEPTION TYPE lcx_delete_not_successfull.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_table_view IMPLEMENTATION.

  METHOD show_table.
    tc_t588z-top_line = 1.
    MOVE-CORRESPONDING it_t588z TO t588z_itab.
    origin_shown_table = it_t588z.
  ENDMETHOD.

  METHOD has_table_changed.

    changed = COND abap_bool( WHEN t588z_itab EQ origin_shown_table THEN abap_false
                              ELSE abap_true ).

  ENDMETHOD.

  METHOD get_shown_table.
    MOVE-CORRESPONDING t588z_itab TO shown.
  ENDMETHOD.

  METHOD pbo.
    me->loop_at_screen( ).
  ENDMETHOD.

  METHOD insert.

    READ TABLE t588z_itab INTO t588z_wa WITH KEY flag = 'X'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CLEAR: t588z_wa-seqno, t588z_wa-kennz, t588z_wa-vinfo.

    INSERT t588z_wa INTO t588z_itab INDEX sy-tabix.
    me->clear_flag( ).

  ENDMETHOD.

  METHOD delete.
    DELETE t588z_itab WHERE flag = abap_true.
  ENDMETHOD.

  METHOD clear_flag.
    LOOP AT t588z_itab ASSIGNING FIELD-SYMBOL(<t588z_line>).
      <t588z_line>-flag = abap_false.
    ENDLOOP.
  ENDMETHOD.

  METHOD insert_flag.
    LOOP AT t588z_itab ASSIGNING FIELD-SYMBOL(<t588z_line>).
      <t588z_line>-flag = abap_true.
    ENDLOOP.
  ENDMETHOD.

  METHOD rebuild_numbers.

    DATA: l_t588z LIKE t588z_wa.

    " maybe there is a way to exclude the lines which are only a comment
    LOOP AT t588z_itab INTO t588z_wa.
      IF l_t588z-subty <> t588z_wa-subty OR
         l_t588z-fname <> t588z_wa-fname OR
         l_t588z-opera <> t588z_wa-opera.
        l_t588z = t588z_wa.
        CLEAR l_t588z-seqno.
      ENDIF.
      l_t588z-seqno = l_t588z-seqno + 1.
      t588z_wa-seqno = l_t588z-seqno.
      MODIFY  t588z_itab FROM t588z_wa INDEX sy-tabix.
    ENDLOOP.

  ENDMETHOD.

  METHOD loop_at_screen.

    LOOP AT SCREEN."INTO DATA(screen).
      " check for edit mode
      IF lcl_general_view=>get_instance( )->get_active_read_mode( ) EQ abap_false. " --> it is change mode

        " the fields should be ready for input
        screen-input = 1.

        IF NOT screen-group1 IS INITIAL.

          " only subtypes
          IF lcl_general_view=>get_instance( )->get_focus( )-ntype = 'S' AND screen-group1 = 'T_S'.
            screen-input = 0.
          ENDIF.

          " Subtypes and Field
          IF lcl_general_view=>get_instance( )->get_focus( )-ntype = 'F' AND ( screen-group1 = 'T_S' OR
                                                                               screen-group1 = 'T_F' ).
            screen-input = 0.
          ENDIF.

          " Subtype, Field and Operation
          IF lcl_general_view=>get_instance( )->get_focus( )-ntype = 'O' AND ( screen-group1 = 'T_S' OR
                                                                               screen-group1 = 'T_F' OR
                                                                               screen-group1 = 'T_O' ).
            screen-input = 0.
          ENDIF.

        ENDIF.
      ELSE.
        " read mode is active - no input allowed
        screen-input = 0.
      ENDIF.

      MODIFY SCREEN." FROM screen.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_origin_table.
    origin = origin_shown_table.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_tree_view IMPLEMENTATION.

  METHOD pbo.

    DATA:
      event       TYPE cntl_simple_event,
      event_table TYPE cntl_simple_events.

    DATA: hierarchy_header TYPE treev_hhdr.

    IF lcl_tree_view=>tree_container IS INITIAL.

      lcl_tree_view=>tree_container = NEW cl_gui_custom_container(
         container_name = 'LCL_TREE_VIEW=>TREE_CONTAINER' ).

      hierarchy_header-heading = 'Gefundene Einträge'(ent).

      tree_grid = NEW cl_gui_column_tree( parent                = lcl_tree_view=>tree_container
                                          node_selection_mode   = cl_gui_column_tree=>node_sel_mode_single
                                          item_selection        = 'X'
                                          hierarchy_column_name = 'Col1'
                                          hierarchy_header      = hierarchy_header ).

      " Double click on node
      CLEAR event_table[].
      event-eventid    = tree_grid->eventid_node_double_click.
      event-appl_event = 'X'. " process PAI if event occurs
      APPEND event TO event_table.

      " double click on item
      event-eventid    = tree_grid->eventid_item_double_click.
      event-appl_event = 'X'. " process PAI if event occurs
      APPEND event TO event_table.

      tree_grid->set_registered_events( event_table ).

      SET HANDLER me->handle_node_double_click FOR tree_grid.
      SET HANDLER me->handle_item_double_click FOR tree_grid.

      " build tree
      tree_grid->add_nodes_and_items( node_table = node_table
                                      item_table = item_table
                                      item_table_structure_name = 'MTREEITM' ).

    ENDIF.

  ENDMETHOD.

  METHOD handle_item_double_click.
    me->handle_node_double_click( node_key ).
  ENDMETHOD.

  METHOD handle_node_double_click.

    DATA t588z TYPE z_t_t588z.

    TRY.
        DATA(keywa) = keytab[ node_key = node_key ].

        IF keywa-subty CS '*'.
          CLEAR keywa-subty.
        ENDIF.
        IF keywa-fname CS '*'.
          CLEAR keywa-fname.
        ENDIF.

        lcl_general_view=>get_instance( )->set_focus( keywa ).

        CASE keywa-ntype.
          WHEN 'I'.
            t588z = lcl_dynmas=>get_instance( )->get_for_infty( infty = keywa-infty ).
          WHEN 'S'.
            t588z = lcl_dynmas=>get_instance( )->get_for_subty( infty = keywa-infty subty = keywa-subty ).
          WHEN 'F'.
            t588z = lcl_dynmas=>get_instance( )->get_for_field( infty = keywa-infty subty = keywa-subty
                                                                field = keywa-fname ).
          WHEN 'O'.
            t588z = lcl_dynmas=>get_instance( )->get_for_actio( infty = keywa-infty subty = keywa-subty
                                                                field = keywa-fname actio = keywa-opera ).
        ENDCASE.

        lcl_table_view=>show_table( t588z ).

      CATCH cx_sy_itab_line_not_found.

    ENDTRY.
  ENDMETHOD.

  METHOD add_node.

    node_counter = node_counter + 1.

    CASE node_type.
      WHEN 'I'.
        me->add_node_as_infty( t588z_line ).
      WHEN 'S'.
        me->add_node_as_subty( t588z_line ).
      WHEN 'F'.
        me->add_node_as_field( t588z_line ).
      WHEN 'O'.
        me->add_node_as_funct( t588z_line ).
      WHEN OTHERS.
        node_counter = node_counter - 1.
    ENDCASE.

  ENDMETHOD.

  METHOD add_node_as_infty.

    DATA node TYPE treev_node.
    DATA txt TYPE string.

    node-node_key = node_counter.
    node-n_image   = '@RG@'.
    node-exp_image = '@RG@'.
    node-isfolder  = 'X'.

    APPEND node TO node_table.

    txt = 'Infotyp' && ` ` && t588z_line-infty.
    me->add_item( node_key = node-node_key txt = txt ).
    me->add_key( t588z_line = t588z_line
                 node_key = node-node_key
                 ntype = 'I'  ).

    CLEAR last_infty_key.
    last_infty_key = node-node_key.

  ENDMETHOD.

  METHOD add_node_as_subty.
    DATA node TYPE treev_node.
    DATA txt  TYPE string.

    node-node_key  = node_counter.
    node-relatkey  = last_infty_key.
    node-relatship = cl_gui_column_tree=>relat_last_child.
    node-n_image   = '@S7@'.
    node-exp_image = '@S7@'.
    node-isfolder  = 'X'.
    node-expander  = 'X'.

    APPEND node TO node_table.

    txt = 'Subtyp' && ` ` && t588z_line-subty.
    me->add_item( node_key = node-node_key txt = txt ).
    me->add_key( t588z_line = t588z_line
                 node_key = node-node_key
                 ntype = 'S'  ).

    CLEAR last_subty_key.
    last_subty_key = node-node_key.

  ENDMETHOD.

  METHOD add_node_as_field.
    DATA node TYPE treev_node.
    DATA txt TYPE string.

    node-node_key  = node_counter.
    node-relatkey  = last_subty_key.
    node-relatship = cl_gui_column_tree=>relat_last_child.
    node-n_image   = '@HO@'.
    node-exp_image = '@HO@'.
    node-isfolder  = 'X'.
    node-expander  = 'X'.

    APPEND node TO node_table.

    txt = 'Feld'(002) && ` ` && t588z_line-fname.
    me->add_item( node_key = node-node_key txt = txt ).
    me->add_key( t588z_line = t588z_line
                 node_key = node-node_key
                 ntype = 'F'  ).

    CLEAR last_fname_key.
    last_fname_key = node-node_key.

  ENDMETHOD.

  METHOD add_node_as_funct.

    DATA node TYPE treev_node.
    DATA txt TYPE string.

    node-node_key  = node_counter.
    node-relatkey  = last_fname_key.
    node-relatship = cl_gui_column_tree=>relat_last_child.
    node-n_image   = '@P6@'.

    APPEND node TO node_table.

    txt = SWITCH string( t588z_line-opera
                          WHEN '00' THEN '00 (generell)'(v00)
                          WHEN '02' THEN '02 (ändern)'(v02)
                          WHEN '04' THEN '04 (anlegen)'(v04)
                          WHEN '06' THEN '06 (anlegen/ändern)'(v06)
                          WHEN '08' THEN '08 (löschen)'(v08)
                          WHEN '10' THEN '10 (ändern/löschen)'(v10)
                          WHEN '12' THEN '12 (anlegen/löschen)'(v12) ).

    me->add_item( node_key = node-node_key txt = txt ).
    me->add_key( t588z_line = t588z_line
                 node_key = node-node_key
                 ntype = 'O'  ).

  ENDMETHOD.

  METHOD add_item.

    " Add Item to Tree
    DATA item TYPE mtreeitm.
    item-node_key = node_key.
    item-item_name = 'Col1'.
    item-class = cl_gui_column_tree=>item_class_text.
    item-text = txt.
    APPEND item TO item_table.

  ENDMETHOD.

  METHOD add_key.

    DATA(keywa) = VALUE t_keytab( node_key = node_key
                                  ntype = ntype ).

    MOVE-CORRESPONDING t588z_line TO keywa.

    APPEND keywa TO keytab.

  ENDMETHOD.

  METHOD get_nodes.
    r_node_table = node_table.
  ENDMETHOD.

  METHOD get_items.
    r_item_table = item_table.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_general_view IMPLEMENTATION.

  METHOD get_instance.
    IF self IS NOT BOUND.
      self = NEW lcl_general_view( ).
    ENDIF.
    r_self = self.
  ENDMETHOD.

  METHOD set_focus.
    focus = s_focus.
  ENDMETHOD.

  METHOD get_focus.
    r_focus = focus.
  ENDMETHOD.

  METHOD save_changes.

    IF me->get_active_read_mode( ) EQ abap_true.
      RETURN.
    ENDIF.

    IF lcl_table_view=>has_table_changed( ).

      lcl_dynmas=>get_instance( )->save( current_t588z = lcl_table_view=>get_shown_table( )
                                         origin_t588z = table_view->get_origin_table( ) ).

      me->change_mode( ).

    ENDIF.

  ENDMETHOD.

  METHOD pbo.
    get_instance( )->init_tree_view( ).
    get_instance( )->init_table_view( ).
  ENDMETHOD.

  METHOD constructor.
    active_read_mode = abap_true.
    tree_view = NEW lcl_tree_view( ).
    table_view = NEW lcl_table_view( ).
  ENDMETHOD.

  METHOD add_node.
    tree_view->add_node( node_type = node_type t588z_line = t588z_line ).
  ENDMETHOD.

  METHOD init_tree_view.
    tree_view->pbo( ).
  ENDMETHOD.

  METHOD init_table_view.
    table_view->pbo( ).
  ENDMETHOD.

  METHOD change_mode.

    " if one of the parameters is supplied!
    IF read_mode IS SUPPLIED.
      active_read_mode = read_mode.
      RETURN.
    ENDIF.
    IF write_mode IS SUPPLIED.
      active_read_mode = COND abap_bool( WHEN write_mode EQ abap_true  THEN abap_false
                                         WHEN write_mode EQ abap_false THEN abap_true ).
      RETURN.
    ENDIF.
    " if none of the parameters is supplied
    active_read_mode = COND abap_bool( WHEN active_read_mode EQ abap_true  THEN abap_false
                                       WHEN active_read_mode EQ abap_false THEN abap_true ).

  ENDMETHOD.

  METHOD get_active_read_mode.
    " if active_read_mode eq abap_false --> write_mode eq true
    r_active_read_mode = active_read_mode.
  ENDMETHOD.

  METHOD insert.
    table_view->insert( ).
  ENDMETHOD.

  METHOD delete.
    table_view->delete( ).
  ENDMETHOD.

  METHOD mark_all.
    table_view->insert_flag( ).
  ENDMETHOD.

  METHOD unmark_all.
    table_view->clear_flag( ).
  ENDMETHOD.

ENDCLASS.