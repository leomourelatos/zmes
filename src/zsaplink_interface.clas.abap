class ZSAPLINK_INTERFACE definition
  public
  inheriting from ZSAPLINK_OO
  create public .

public section.

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
protected section.

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.

  constants XML_KEY_METHOD type STRING value 'method' ##NO_TEXT.
  constants XML_KEY_PARAMETER type STRING value 'parameter' ##NO_TEXT.
  constants XML_KEY_EXCEPTION type STRING value 'exception' ##NO_TEXT.
  constants XML_KEY_EVENTS type STRING value 'events' ##NO_TEXT.
  constants XML_KEY_ATTRIBUTE type STRING value 'attribute' ##NO_TEXT.
  constants XML_KEY_TYPEPUSAGE type STRING value 'typeUsage' ##NO_TEXT.
  constants XML_KEY_CLSDEFERRD type STRING value 'typeClasDef' ##NO_TEXT.
  constants XML_KEY_INTDEFERRD type STRING value 'typeIntfDef' ##NO_TEXT.
  constants XML_KEY_TYPES type STRING value 'types' ##NO_TEXT.
  constants XML_KEY_INCLUDE type STRING value 'include' ##NO_TEXT.

  methods EXPORT_INTERFACES
    importing
      !CLASSKEY type SEOCLSKEY
    exporting
      !RC type SYSUBRC
    changing
      !ROOTNODE type ref to IF_IXML_ELEMENT .
  methods IMPORT_METHODS
    changing
      !CH_METHODS type SEOO_METHODS_R
      !CH_PARAMETERS type SEOS_PARAMETERS_R
      !CH_EXCEPS type SEOS_EXCEPTIONS_R .
  methods IMPORT_ATTRIBUTES
    changing
      !CH_ATTRIBUTES type SEOO_ATTRIBUTES_R
    raising
      ZCX_SAPLINK .
  methods IMPORT_INTERFACES
    changing
      !CH_COMPRISINGS type SEOR_COMPRISINGS_R .
  methods IMPORT_TYPES
    changing
      !CH_TYPES type SEOO_TYPES_R .
  methods IMPORT_EVENTS
    changing
      !CH_EVENTS type SEOO_EVENTS_R
      !CH_PARAMETERS type SEOS_PARAMETERS_R .
  methods EXPORT_METHODS
    importing
      !INTFDESCR type ref to CL_ABAP_INTFDESCR
      !CLASSNAME type SEOCLSNAME
    changing
      !RC type SYSUBRC
      !ROOTNODE type ref to IF_IXML_ELEMENT .
  methods EXPORT_EVENTS
    importing
      !CLASSKEY type SEOCLSKEY
    changing
      !RC type SYSUBRC
      !ROOTNODE type ref to IF_IXML_ELEMENT .
  methods EXPORT_ATTRIBUTES
    importing
      !INTFDESCR type ref to CL_ABAP_INTFDESCR
    changing
      !RC type SYSUBRC
      !ROOTNODE type ref to IF_IXML_ELEMENT .
  methods EXPORT_TYPES
    importing
      !CLASSKEY type SEOCLSKEY
    changing
      !RC type SYSUBRC
      !ROOTNODE type ref to IF_IXML_ELEMENT .
ENDCLASS.



CLASS ZSAPLINK_INTERFACE IMPLEMENTATION.


METHOD CHECKEXISTS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA intkey TYPE seoclskey.
  DATA not_active TYPE  char1.

  intkey-clsname = objname.

  CALL FUNCTION 'SEO_INTERFACE_EXISTENCE_CHECK'
    EXPORTING
      intkey              = intkey
    IMPORTING
      not_active    = not_active
    EXCEPTIONS
*     NOT_SPECIFIED       = 1
     not_existing        = 2
*     IS_CLASS            = 3
*     NO_TEXT             = 4
*     INCONSISTENT        = 5
*     OTHERS              = 6
            .
  IF sy-subrc <> 2.
    exists = 'X'.
  ENDIF.
ENDMETHOD.


METHOD CREATEIXMLDOCFROMOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA intfsection TYPE REF TO if_ixml_element.
  DATA rootnode TYPE REF TO if_ixml_element.
  DATA _classname TYPE seoclsname.
  DATA rc TYPE sysubrc.
  DATA intfdescr TYPE REF TO cl_abap_intfdescr.
  DATA typedescr TYPE REF TO cl_abap_typedescr.
  DATA classkey TYPE seoclskey.
  DATA intproperties TYPE vseointerf.
  DATA _objtype TYPE string.

  _classname = objname.
  classkey-clsname = objname.

  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  CALL FUNCTION 'SEO_INTERFACE_GET'
    EXPORTING
      intkey       = classkey
      version      = '1'
    IMPORTING
      interface    = intproperties
    EXCEPTIONS
      not_existing = 1
      deleted      = 2
      is_class     = 3
      model_only   = 4
      OTHERS       = 5.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>not_found.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'interface deleted'.
      WHEN 3.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'classes not supported'.
      WHEN 4.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'interface is modeled only'.
    ENDCASE.
  ENDIF.

  setattributesfromstructure( node = rootnode
                         structure = intproperties ).

  TRY.
      CALL METHOD cl_abap_intfdescr=>describe_by_name
        EXPORTING
          p_name         = objname
        RECEIVING
          p_descr_ref    = typedescr
        EXCEPTIONS
          type_not_found = 1.
      IF sy-subrc = 0.
        intfdescr ?= typedescr.
      ENDIF.
    CATCH cx_root.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>system_error.
  ENDTRY.

*Add included interfaces to the xml document
  CALL METHOD me->export_interfaces
    EXPORTING
      classkey = classkey
    IMPORTING
      rc       = rc
    CHANGING
      rootnode = rootnode.

*Add types to the xml document
  CALL METHOD me->export_types
    EXPORTING
      classkey = classkey
    CHANGING
      rc       = rc
      rootnode = rootnode.

*Add type pool groups to the xml document
  CALL METHOD me->get_typepusage
    CHANGING
      xo_rootnode = rootnode.

*Add class deferred type groups to the xml document
  CALL METHOD me->get_clsdeferrd
    CHANGING
      xo_rootnode = rootnode.

*Add interface deferred type groups to the xml document
  CALL METHOD me->get_intdeferrd
    CHANGING
      xo_rootnode = rootnode.

*Add events to the xml document
  CALL METHOD me->export_events
    EXPORTING
      classkey = classkey
    CHANGING
      rc       = rc
      rootnode = rootnode.

*Add attributes to the xml document
  CALL METHOD me->export_attributes
    EXPORTING
      intfdescr = intfdescr
    CHANGING
      rc        = rc
      rootnode  = rootnode.

*Add methods to the xml document
  CALL METHOD me->export_methods
    EXPORTING
      intfdescr = intfdescr
      classname = _classname
    CHANGING
      rc        = rc
      rootnode  = rootnode.

* create alias info for load.
  get_alias_method( EXPORTING it_methods     = intfdescr->methods
                    CHANGING  xo_rootnode    = rootnode ).

* append root node to xmldoc
  rc = xmldoc->append_child( rootnode ).
  ixmldocument = xmldoc.
ENDMETHOD.


METHOD CREATEOBJECTFROMIXMLDOC.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA rootnode TYPE REF TO if_ixml_element.
  DATA classkey TYPE seoclskey.
  DATA not_active TYPE boolean.
  DATA _devclass TYPE devclass.
  DATA _objtype TYPE string.
  DATA checkexists TYPE flag.

  DATA: e_corrnr                 TYPE trkorr,
        e_devclass               TYPE devclass,
        e_version                TYPE seoversion,
        e_genflag                TYPE genflag,
        e_authority_check        TYPE seox_boolean,
        e_overwrite              TYPE seox_boolean.

  DATA: i_korrnr TYPE trkorr.

  DATA: ch_interface TYPE vseointerf,
        ch_comprisings TYPE seor_comprisings_r,
        ch_attributes TYPE seoo_attributes_r,
        ch_methods TYPE seoo_methods_r,
        ch_events TYPE seoo_events_r,
        ch_parameters TYPE seos_parameters_r,
        ch_exceps TYPE seos_exceptions_r,
        ch_typepusages TYPE seot_typepusages_r,
        ch_clsdeferrds TYPE seot_clsdeferrds_r,
        ch_intdeferrds TYPE seot_intdeferrds_r,
        ch_aliases     TYPE seoo_aliases_r ,
        ch_types TYPE seoo_types_r.

  CALL FUNCTION 'SEO_BUFFER_INIT'.

  e_devclass = devclass.
  _objtype = getobjecttype( ).
  e_overwrite = overwrite.

  _devclass = devclass.
  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = ch_interface.

  checkexists = checkexists( ).
  IF checkexists IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>existing.
    ELSE.
*     delete object for new install
      deleteobject( ).
    ENDIF.
  ENDIF.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>not_authorized.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>system_error.
    ENDCASE.
  ENDIF.

*Add attributes to new interface
  CALL METHOD import_attributes
    CHANGING
      ch_attributes = ch_attributes.

*Add includes
  CALL METHOD import_interfaces
    CHANGING
      ch_comprisings = ch_comprisings.

*Add types
  CALL METHOD import_types
    CHANGING
      ch_types = ch_types.

*Add type pool groups
  CALL METHOD create_typepusage
    CHANGING
      xt_typepusages = ch_typepusages.
*Add class deferred types
  CALL METHOD create_clsdeferrd
    CHANGING
      xt_clsdeferrds = ch_clsdeferrds.
*Add interface deferred types
  CALL METHOD create_intdeferrd
    CHANGING
      xt_intdeferrds = ch_intdeferrds.

*Add Aliases for methods
  CALL METHOD create_alias_method
    CHANGING
      xt_aliases_method = ch_aliases.

*Add events and event parameters
  CALL METHOD import_events
    CHANGING
      ch_events     = ch_events
      ch_parameters = ch_parameters.

*Add methods, method parameters and method exceptions
  CALL METHOD import_methods
    CHANGING
      ch_methods    = ch_methods
      ch_parameters = ch_parameters
      ch_exceps     = ch_exceps.

*Create the interface
  CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
   EXPORTING
    corrnr                             = e_corrnr
    devclass                           = e_devclass
    version                            = e_version
    genflag                            = e_genflag
    authority_check                    = e_authority_check
    overwrite                          = e_overwrite
*     SUPPRESS_REFACTORING_SUPPORT       = SEOX_TRUE
   IMPORTING
    korrnr                             = i_korrnr
* TABLES
*   CLASS_DESCRIPTIONS                 =
*   COMPONENT_DESCRIPTIONS             =
*   SUBCOMPONENT_DESCRIPTIONS          =
   CHANGING
    interface                          = ch_interface
    comprisings                        = ch_comprisings
    attributes                         = ch_attributes
    methods                            = ch_methods
    events                             = ch_events
    PARAMETERS                         = ch_parameters
    exceps                             = ch_exceps
    aliases                            = ch_aliases
    typepusages                        = ch_typepusages
    clsdeferrds                        = ch_clsdeferrds
    intdeferrds                        = ch_intdeferrds
    types                              = ch_types
   EXCEPTIONS
    existing                           = 1
    is_class                           = 2
    db_error                           = 3
    component_error                    = 4
    no_access                          = 5
    other                              = 6
    OTHERS                             = 7.

  CASE sy-subrc.
    WHEN '0'.
** i guess if we made it this far, we will assume
** successful install
      name = objname.
    WHEN '1'.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>existing.
    WHEN OTHERS.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>system_error.
  ENDCASE.

ENDMETHOD.


METHOD DELETEOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA clskey TYPE seoclskey.
  clskey-clsname = objname.

  CALL FUNCTION 'SEO_INTERFACE_GET'
    EXPORTING
      intkey       = clskey
      version      = seoc_version_inactive
      state        = '0'
    EXCEPTIONS
      not_existing = 1
      deleted      = 2
      is_class     = 3
      model_only   = 4
      OTHERS       = 5.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg = 'interface not deleted'.
  ENDIF.

  CALL FUNCTION 'SEO_INTERFACE_DELETE_W_DEPS'
    EXPORTING
      intkey       = clskey
      save         = ' '
    EXCEPTIONS
      not_existing = 1
      is_class     = 2
      not_deleted  = 3
      db_error     = 4
      OTHERS       = 5.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>not_found.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'class not supported'.
      WHEN 3.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'interface not deleted'.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>system_error.
    ENDCASE.
  ENDIF.

  CALL FUNCTION 'SEO_CLIF_SAVE_ALL'
    EXPORTING
      cifkey        = clskey
*      CHANGING
*        CORRNR        = corrnr
    EXCEPTIONS
      not_existing  = 1
      nothing_to_do = 2
      access_error  = 3
      db_error      = 4
      OTHERS        = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMETHOD.


METHOD EXPORT_ATTRIBUTES.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA: attribkey TYPE seocmpkey,
        attribdescr TYPE abap_attrdescr,
        attribnode TYPE REF TO if_ixml_element,
        attribproperties TYPE vseoattrib,
        _otrguid TYPE sotr_conc,
        otrnode TYPE REF TO if_ixml_element.

  attribkey-clsname = objname.
  LOOP AT intfdescr->attributes INTO attribdescr WHERE is_inherited =
  abap_false.
    attribnode = xmldoc->create_element( xml_key_attribute ).
    attribkey-cmpname = attribdescr-name.
    CALL FUNCTION 'SEO_ATTRIBUTE_GET'
      EXPORTING
        attkey    = attribkey
      IMPORTING
        attribute = attribproperties.

*   include OTR if necessary (for exception classes)
    IF attribproperties-type = 'SOTR_CONC' AND attribproperties-attvalue
    IS NOT INITIAL.
      _otrguid = attribproperties-attvalue+1(32).
      otrnode = get_otr( _otrguid ).
      IF otrnode IS BOUND.
        rc = attribnode->append_child( otrnode ).
      ENDIF.
    ENDIF.

*   append attribute node to parent node
    setattributesfromstructure( node = attribnode structure =
    attribproperties ).
    rc = rootnode->append_child( attribnode ).
  ENDLOOP.

ENDMETHOD.


METHOD EXPORT_EVENTS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA: events      TYPE seoo_events_r,
        wa_event    LIKE LINE OF events,
        eventkey    TYPE seocmpkey,
        eventparams TYPE seos_parameters_r,
        wa_params   TYPE seos_parameter_r,
        event_node  TYPE REF TO if_ixml_element,
        parameternode TYPE REF TO if_ixml_element.

  CALL FUNCTION 'SEO_EVENT_READ_ALL'
    EXPORTING
      cifkey            = classkey
      version           = 1
    IMPORTING
      events            = events
    EXCEPTIONS
      clif_not_existing = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.
  ENDIF.

  LOOP AT events INTO wa_event.
    eventkey-clsname = wa_event-clsname.
    eventkey-cmpname = wa_event-cmpname.
    event_node = xmldoc->create_element( xml_key_events ).
    setattributesfromstructure( node = event_node structure =
    wa_event ).
    CALL FUNCTION 'SEO_EVENT_SIGNATURE_GET'
      EXPORTING
        evtkey     = eventkey
      IMPORTING
        PARAMETERS = eventparams.
*   event parameters
    LOOP AT eventparams INTO wa_params.

      parameternode = xmldoc->create_element( xml_key_parameter ).
      setattributesfromstructure( node = parameternode
      structure = wa_params ).
      rc = event_node->append_child( parameternode ).
    ENDLOOP.
    rc = rootnode->append_child( event_node ).
  ENDLOOP.

ENDMETHOD.


METHOD EXPORT_INTERFACES.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA: it_vseocompri TYPE TABLE OF vseocompri,
        wa_vseocompri LIKE LINE OF it_vseocompri,
        implementingnode TYPE REF TO if_ixml_element,
        objname TYPE vrsd-objname.

  objname = classkey.

  CALL FUNCTION 'SVRS_GET_VERSION_INTF_40'
    EXPORTING
*   DESTINATION                        =
      object_name                        = objname
      versno                             = '00000'
*   IV_NO_RELEASE_TRANSFORMATION       =
* IMPORTING
*   INFO_LINE                          =
  TABLES
*   VSMODISRC                          =
*   PSEOALIASES                        =
*   PVSEOATTRIB                        =
      pvseocompri                        = it_vseocompri
*   PVSEOEVENT                         =
*   PVSEOEXCEP                         =
*   PVSEOINTERF                        =
*   PSMODILOG                          =
*   PVSEOMETHOD                        =
*   PVSEOPARAM                         =
*   PPOOL_SOURCE                       =
*   PSOURCE                            =
*   PTRDIR                             =
*   TYPE_TAB                           =
*   PSEOTYPEPLS                        =
  EXCEPTIONS
    no_version                         = 1
    system_failure                     = 2
    communication_failure              = 3
    OTHERS                             = 4
            .
  IF sy-subrc <> 0.
  ENDIF.

  LOOP AT it_vseocompri INTO wa_vseocompri.
    implementingnode = xmldoc->create_element( xml_key_include ).
    setattributesfromstructure( node = implementingnode structure =
    wa_vseocompri ).
    rc = rootnode->append_child( implementingnode ).
  ENDLOOP.

ENDMETHOD.


METHOD EXPORT_METHODS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA: methoddescr TYPE abap_methdescr,
        methodkey TYPE seocpdkey,
        clsmethkey TYPE seocmpkey,
        methodproperties TYPE vseomethod,
        paramdescr TYPE abap_parmdescr,
        paramproperties TYPE vseoparam,
        paramkey TYPE seoscokey,
        exceptionlist TYPE seos_exceptions_r,
        anexception TYPE vseoexcep,
        exceptionnode TYPE REF TO if_ixml_element,
        parameternode TYPE REF TO if_ixml_element,
        methodnode TYPE REF TO if_ixml_element.

  LOOP AT intfdescr->methods INTO methoddescr WHERE alias_for IS INITIAL AND
  NOT ( is_inherited = 'X' AND is_redefined IS INITIAL ).
    methodkey-clsname = classname.
    methodkey-cpdname = methoddescr-name.

    clsmethkey-clsname = classname.
    clsmethkey-cmpname = methoddescr-name.
    CLEAR methodproperties.

    CALL FUNCTION 'SEO_METHOD_GET'
      EXPORTING
        mtdkey       = clsmethkey
      IMPORTING
        method       = methodproperties
      EXCEPTIONS
        not_existing = 1.
    IF sy-subrc = 0.
      methodnode = xmldoc->create_element( xml_key_method ).
      setattributesfromstructure( node = methodnode structure =
      methodproperties ).

      LOOP AT methoddescr-parameters INTO paramdescr.
        CLEAR paramproperties.
        parameternode = xmldoc->create_element( xml_key_parameter ).
        paramkey-cmpname = clsmethkey-cmpname.
        paramkey-sconame = paramdescr-name.
        paramkey-clsname = objname.
        CALL FUNCTION 'SEO_PARAMETER_GET'
          EXPORTING
            parkey    = paramkey
            version   = '1'
          IMPORTING
            parameter = paramproperties.
        setattributesfromstructure( node = parameternode
        structure = paramproperties ).
        rc = methodnode->append_child( parameternode ).
      ENDLOOP.

*add method exceptions
      CALL FUNCTION 'SEO_METHOD_SIGNATURE_GET'
        EXPORTING
          mtdkey  = clsmethkey
          version = '1'
        IMPORTING
          exceps  = exceptionlist.
      LOOP AT exceptionlist INTO anexception.
        exceptionnode = xmldoc->create_element( xml_key_exception ).
        setattributesfromstructure( node = exceptionnode
        structure = anexception ).
        rc = methodnode->append_child( exceptionnode ).
      ENDLOOP.
    ENDIF. "method found
    rc = rootnode->append_child( methodnode ).
  ENDLOOP.

ENDMETHOD.


METHOD EXPORT_TYPES.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA: types      TYPE seoo_types_r,
      wa_type    LIKE LINE OF types,
      types_node TYPE REF TO if_ixml_element.

  CALL FUNCTION 'SEO_TYPE_READ_ALL'
    EXPORTING
      cifkey            = classkey
      version           = 1
    IMPORTING
      types             = types
    EXCEPTIONS
      clif_not_existing = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.
  ENDIF.

  LOOP AT types INTO wa_type.
    types_node = xmldoc->create_element( xml_key_types ).
    setattributesfromstructure( node = types_node structure =
    wa_type ).
    rc = rootnode->append_child( types_node ).
  ENDLOOP.

ENDMETHOD.


METHOD GETOBJECTTYPE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/

  objecttype = 'INTF'.  "Interface

ENDMETHOD.


METHOD IMPORT_ATTRIBUTES.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA: otrconcept TYPE sotr_text-concept,
        wa_attributes LIKE LINE OF ch_attributes,
        filter TYPE REF TO if_ixml_node_filter,
        iterator TYPE REF TO if_ixml_node_iterator,
        node TYPE REF TO if_ixml_element,
        otrnode TYPE REF TO if_ixml_element.

  filter = xmldoc->create_filter_name( xml_key_attribute ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
*   create OTR texts if necessary (for exception classes)
    CLEAR otrconcept.
    otrnode = node->find_from_name( 'sotr' ).
    IF otrnode IS NOT INITIAL.
      me->create_otr(
        EXPORTING node = otrnode
        IMPORTING concept = otrconcept ).
    ENDIF.
    CLEAR wa_attributes.
*   create attribute
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_attributes.
    wa_attributes-version = '0'.
*   ewH:issue33-->6.40 and above, must create new concept
    IF otrconcept IS NOT INITIAL.
      CONCATENATE `'` otrconcept `'` INTO wa_attributes-attvalue.
    ENDIF.
    APPEND wa_attributes TO ch_attributes.
    node ?= iterator->get_next( ).
  ENDWHILE.

ENDMETHOD.


METHOD IMPORT_EVENTS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA: event_filter TYPE REF TO if_ixml_node_filter,
        parameter_filter TYPE REF TO if_ixml_node_filter,
        event_iterator TYPE REF TO if_ixml_node_iterator,
        parameter_iterator TYPE REF TO if_ixml_node_iterator,
        event_node TYPE REF TO if_ixml_element,
        parameter_node TYPE REF TO if_ixml_element,
        wa_events LIKE LINE OF ch_events,
        wa_parameters LIKE LINE OF ch_parameters.

  event_filter = xmldoc->create_filter_name( xml_key_events ).
  event_iterator = xmldoc->create_iterator_filtered( event_filter ).
  event_node ?= event_iterator->get_next( ).
  WHILE event_node IS NOT INITIAL.
    CLEAR wa_events.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = event_node
      CHANGING
        structure = wa_events.
    APPEND wa_events TO ch_events.
    parameter_filter = event_node->create_filter_name( xml_key_parameter ).
    parameter_iterator = event_node->create_iterator_filtered( parameter_filter ).
    parameter_node ?= parameter_iterator->get_next( ).
    WHILE parameter_node IS NOT INITIAL.
      CLEAR wa_parameters.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = parameter_node
        CHANGING
          structure = wa_parameters.
      APPEND wa_parameters TO ch_parameters.
      parameter_node ?= parameter_iterator->get_next( ).
    ENDWHILE.
    event_node ?= event_iterator->get_next( ).
  ENDWHILE.

ENDMETHOD.


METHOD IMPORT_INTERFACES.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA: filter TYPE REF TO if_ixml_node_filter,
        iterator TYPE REF TO if_ixml_node_iterator,
        node TYPE REF TO if_ixml_element,
        wa_comprisings LIKE LINE OF ch_comprisings.

  filter = xmldoc->create_filter_name( xml_key_include ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR wa_comprisings.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_comprisings.
    wa_comprisings-version = '0'.
    APPEND wa_comprisings TO ch_comprisings.
    node ?= iterator->get_next( ).
  ENDWHILE.

ENDMETHOD.


METHOD IMPORT_METHODS.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA: filter TYPE REF TO if_ixml_node_filter,
        filter2 TYPE REF TO if_ixml_node_filter,
        iterator TYPE REF TO if_ixml_node_iterator,
        iterator2 TYPE REF TO if_ixml_node_iterator,
        node TYPE REF TO if_ixml_element,
        node2 TYPE REF TO if_ixml_element,
        wa_parameters LIKE LINE OF ch_parameters,
        wa_methods LIKE LINE OF ch_methods,
        wa_exceps LIKE LINE OF ch_exceps.

*Add methods to new interface
  filter = xmldoc->create_filter_name( xml_key_method ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR wa_methods.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_methods.

*Add parameters
    filter2 = node->create_filter_name( xml_key_parameter ).
    iterator2 = node->create_iterator_filtered( filter2 ).
    node2 ?= iterator2->get_next( ).
    WHILE node2 IS NOT INITIAL.
      CLEAR wa_parameters.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node2
        CHANGING
          structure = wa_parameters.
      APPEND wa_parameters TO ch_parameters.
      node2 ?= iterator2->get_next( ).
    ENDWHILE.
*Add exceptions
    filter2 = node->create_filter_name( xml_key_exception ).
    iterator2 = node->create_iterator_filtered( filter2 ).
    node2 ?= iterator2->get_next( ).
    WHILE node2 IS NOT INITIAL.
      CALL METHOD getstructurefromattributes
        EXPORTING
          node      = node2
        CHANGING
          structure = wa_exceps.
      APPEND wa_exceps TO ch_exceps.
      node2 ?= iterator2->get_next( ).
    ENDWHILE.
    APPEND wa_methods TO ch_methods.
    node ?= iterator->get_next( ).
  ENDWHILE.

ENDMETHOD.


METHOD IMPORT_TYPES.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   The code of this project is provided to you under the current     |
*|   version of the SAP Code Exchange Terms of Use. You can find the   |
*|   text on the SAP Code Exchange webpage at http://www.sdn.sap.com   |
*|                                                                     |
*|   SAPlink is provided to you AS IS with no guarantee, warranty or   |
*|   support.                                                          |
*\---------------------------------------------------------------------/
  DATA: filter TYPE REF TO if_ixml_node_filter,
        iterator TYPE REF TO if_ixml_node_iterator,
        node TYPE REF TO if_ixml_element,
        wa_types       LIKE LINE OF ch_types.

  filter = xmldoc->create_filter_name( xml_key_types ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  WHILE node IS NOT INITIAL.
    CLEAR wa_types.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_types.
    wa_types-version = '0'.
    APPEND wa_types TO ch_types.
    node ?= iterator->get_next( ).
  ENDWHILE.

ENDMETHOD.
ENDCLASS.
