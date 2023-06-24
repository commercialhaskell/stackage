-- Automatically generated from /src/postgresql-12.0/src/src/backend/utils/errcodes.txt using errcodes 2019-10-05 16:38:46.694932074 UTC.
{-# LANGUAGE OverloadedStrings #-}
-- |PostgreSQL error codes.
module Database.PostgreSQL.Typed.ErrCodes (names
  -- * Class 00 - Successful Completion
  , successful_completion
  -- * Class 01 - Warning
  , warning
  , warning_dynamic_result_sets_returned
  , warning_implicit_zero_bit_padding
  , warning_null_value_eliminated_in_set_function
  , warning_privilege_not_granted
  , warning_privilege_not_revoked
  , warning_string_data_right_truncation
  , warning_deprecated_feature
  -- * Class 02 - No Data (this is also a warning class per the SQL standard)
  , no_data
  , no_additional_dynamic_result_sets_returned
  -- * Class 03 - SQL Statement Not Yet Complete
  , sql_statement_not_yet_complete
  -- * Class 08 - Connection Exception
  , connection_exception
  , connection_does_not_exist
  , connection_failure
  , sqlclient_unable_to_establish_sqlconnection
  , sqlserver_rejected_establishment_of_sqlconnection
  , transaction_resolution_unknown
  , protocol_violation
  -- * Class 09 - Triggered Action Exception
  , triggered_action_exception
  -- * Class 0A - Feature Not Supported
  , feature_not_supported
  -- * Class 0B - Invalid Transaction Initiation
  , invalid_transaction_initiation
  -- * Class 0F - Locator Exception
  , locator_exception
  , invalid_locator_specification
  -- * Class 0L - Invalid Grantor
  , invalid_grantor
  , invalid_grant_operation
  -- * Class 0P - Invalid Role Specification
  , invalid_role_specification
  -- * Class 0Z - Diagnostics Exception
  , diagnostics_exception
  , stacked_diagnostics_accessed_without_active_handler
  -- * Class 20 - Case Not Found
  , case_not_found
  -- * Class 21 - Cardinality Violation
  , cardinality_violation
  -- * Class 22 - Data Exception
  , data_exception
  , _ARRAY_ELEMENT_ERROR
  , array_subscript_error
  , character_not_in_repertoire
  , datetime_field_overflow
  , _DATETIME_VALUE_OUT_OF_RANGE
  , division_by_zero
  , error_in_assignment
  , escape_character_conflict
  , indicator_overflow
  , interval_field_overflow
  , invalid_argument_for_logarithm
  , invalid_argument_for_ntile_function
  , invalid_argument_for_nth_value_function
  , invalid_argument_for_power_function
  , invalid_argument_for_width_bucket_function
  , invalid_character_value_for_cast
  , invalid_datetime_format
  , invalid_escape_character
  , invalid_escape_octet
  , invalid_escape_sequence
  , nonstandard_use_of_escape_character
  , invalid_indicator_parameter_value
  , invalid_parameter_value
  , invalid_preceding_or_following_size
  , invalid_regular_expression
  , invalid_row_count_in_limit_clause
  , invalid_row_count_in_result_offset_clause
  , invalid_tablesample_argument
  , invalid_tablesample_repeat
  , invalid_time_zone_displacement_value
  , invalid_use_of_escape_character
  , most_specific_type_mismatch
  , null_value_not_allowed
  , null_value_no_indicator_parameter
  , numeric_value_out_of_range
  , sequence_generator_limit_exceeded
  , string_data_length_mismatch
  , string_data_right_truncation
  , substring_error
  , trim_error
  , unterminated_c_string
  , zero_length_character_string
  , floating_point_exception
  , invalid_text_representation
  , invalid_binary_representation
  , bad_copy_file_format
  , untranslatable_character
  , not_an_xml_document
  , invalid_xml_document
  , invalid_xml_content
  , invalid_xml_comment
  , invalid_xml_processing_instruction
  , duplicate_json_object_key_value
  , invalid_json_text
  , invalid_sql_json_subscript
  , more_than_one_sql_json_item
  , no_sql_json_item
  , non_numeric_sql_json_item
  , non_unique_keys_in_a_json_object
  , singleton_sql_json_item_required
  , sql_json_array_not_found
  , sql_json_member_not_found
  , sql_json_number_not_found
  , sql_json_object_not_found
  , too_many_json_array_elements
  , too_many_json_object_members
  , sql_json_scalar_required
  -- * Class 23 - Integrity Constraint Violation
  , integrity_constraint_violation
  , restrict_violation
  , not_null_violation
  , foreign_key_violation
  , unique_violation
  , check_violation
  , exclusion_violation
  -- * Class 24 - Invalid Cursor State
  , invalid_cursor_state
  -- * Class 25 - Invalid Transaction State
  , invalid_transaction_state
  , active_sql_transaction
  , branch_transaction_already_active
  , held_cursor_requires_same_isolation_level
  , inappropriate_access_mode_for_branch_transaction
  , inappropriate_isolation_level_for_branch_transaction
  , no_active_sql_transaction_for_branch_transaction
  , read_only_sql_transaction
  , schema_and_data_statement_mixing_not_supported
  , no_active_sql_transaction
  , in_failed_sql_transaction
  , idle_in_transaction_session_timeout
  -- * Class 26 - Invalid SQL Statement Name
  , invalid_sql_statement_name
  -- * Class 27 - Triggered Data Change Violation
  , triggered_data_change_violation
  -- * Class 28 - Invalid Authorization Specification
  , invalid_authorization_specification
  , invalid_password
  -- * Class 2B - Dependent Privilege Descriptors Still Exist
  , dependent_privilege_descriptors_still_exist
  , dependent_objects_still_exist
  -- * Class 2D - Invalid Transaction Termination
  , invalid_transaction_termination
  -- * Class 2F - SQL Routine Exception
  , sql_routine_exception
  , s_r_e_function_executed_no_return_statement
  , s_r_e_modifying_sql_data_not_permitted
  , s_r_e_prohibited_sql_statement_attempted
  , s_r_e_reading_sql_data_not_permitted
  -- * Class 34 - Invalid Cursor Name
  , invalid_cursor_name
  -- * Class 38 - External Routine Exception
  , external_routine_exception
  , e_r_e_containing_sql_not_permitted
  , e_r_e_modifying_sql_data_not_permitted
  , e_r_e_prohibited_sql_statement_attempted
  , e_r_e_reading_sql_data_not_permitted
  -- * Class 39 - External Routine Invocation Exception
  , external_routine_invocation_exception
  , e_r_i_e_invalid_sqlstate_returned
  , e_r_i_e_null_value_not_allowed
  , e_r_i_e_trigger_protocol_violated
  , e_r_i_e_srf_protocol_violated
  , e_r_i_e_event_trigger_protocol_violated
  -- * Class 3B - Savepoint Exception
  , savepoint_exception
  , invalid_savepoint_specification
  -- * Class 3D - Invalid Catalog Name
  , invalid_catalog_name
  -- * Class 3F - Invalid Schema Name
  , invalid_schema_name
  -- * Class 40 - Transaction Rollback
  , transaction_rollback
  , transaction_integrity_constraint_violation
  , serialization_failure
  , statement_completion_unknown
  , deadlock_detected
  -- * Class 42 - Syntax Error or Access Rule Violation
  , syntax_error_or_access_rule_violation
  , syntax_error
  , insufficient_privilege
  , cannot_coerce
  , grouping_error
  , windowing_error
  , invalid_recursion
  , invalid_foreign_key
  , invalid_name
  , name_too_long
  , reserved_name
  , datatype_mismatch
  , indeterminate_datatype
  , collation_mismatch
  , indeterminate_collation
  , wrong_object_type
  , generated_always
  , undefined_column
  , _UNDEFINED_CURSOR
  , _UNDEFINED_DATABASE
  , undefined_function
  , _UNDEFINED_PSTATEMENT
  , _UNDEFINED_SCHEMA
  , undefined_table
  , undefined_parameter
  , undefined_object
  , duplicate_column
  , duplicate_cursor
  , duplicate_database
  , duplicate_function
  , duplicate_prepared_statement
  , duplicate_schema
  , duplicate_table
  , duplicate_alias
  , duplicate_object
  , ambiguous_column
  , ambiguous_function
  , ambiguous_parameter
  , ambiguous_alias
  , invalid_column_reference
  , invalid_column_definition
  , invalid_cursor_definition
  , invalid_database_definition
  , invalid_function_definition
  , invalid_prepared_statement_definition
  , invalid_schema_definition
  , invalid_table_definition
  , invalid_object_definition
  -- * Class 44 - WITH CHECK OPTION Violation
  , with_check_option_violation
  -- * Class 53 - Insufficient Resources
  , insufficient_resources
  , disk_full
  , out_of_memory
  , too_many_connections
  , configuration_limit_exceeded
  -- * Class 54 - Program Limit Exceeded
  , program_limit_exceeded
  , statement_too_complex
  , too_many_columns
  , too_many_arguments
  -- * Class 55 - Object Not In Prerequisite State
  , object_not_in_prerequisite_state
  , object_in_use
  , cant_change_runtime_param
  , lock_not_available
  , unsafe_new_enum_value_usage
  -- * Class 57 - Operator Intervention
  , operator_intervention
  , query_canceled
  , admin_shutdown
  , crash_shutdown
  , cannot_connect_now
  , database_dropped
  -- * Class 58 - System Error (errors external to PostgreSQL itself)
  , system_error
  , io_error
  , undefined_file
  , duplicate_file
  -- * Class 72 - Snapshot Failure
  , snapshot_too_old
  -- * Class F0 - Configuration File Error
  , config_file_error
  , lock_file_exists
  -- * Class HV - Foreign Data Wrapper Error (SQL/MED)
  , fdw_error
  , fdw_column_name_not_found
  , fdw_dynamic_parameter_value_needed
  , fdw_function_sequence_error
  , fdw_inconsistent_descriptor_information
  , fdw_invalid_attribute_value
  , fdw_invalid_column_name
  , fdw_invalid_column_number
  , fdw_invalid_data_type
  , fdw_invalid_data_type_descriptors
  , fdw_invalid_descriptor_field_identifier
  , fdw_invalid_handle
  , fdw_invalid_option_index
  , fdw_invalid_option_name
  , fdw_invalid_string_length_or_buffer_length
  , fdw_invalid_string_format
  , fdw_invalid_use_of_null_pointer
  , fdw_too_many_handles
  , fdw_out_of_memory
  , fdw_no_schemas
  , fdw_option_name_not_found
  , fdw_reply_handle
  , fdw_schema_not_found
  , fdw_table_not_found
  , fdw_unable_to_create_execution
  , fdw_unable_to_create_reply
  , fdw_unable_to_establish_connection
  -- * Class P0 - PL/pgSQL Error
  , plpgsql_error
  , raise_exception
  , no_data_found
  , too_many_rows
  , assert_failure
  -- * Class XX - Internal Error
  , internal_error
  , data_corrupted
  , index_corrupted
) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map, fromDistinctAscList)

-- |@SUCCESSFUL_COMPLETION@: 00000 (Success)
successful_completion :: ByteString
successful_completion = "00000"

-- |@WARNING@: 01000 (Warning)
warning :: ByteString
warning = "01000"

-- |@WARNING_DYNAMIC_RESULT_SETS_RETURNED@: 0100C (Warning)
warning_dynamic_result_sets_returned :: ByteString
warning_dynamic_result_sets_returned = "0100C"

-- |@WARNING_IMPLICIT_ZERO_BIT_PADDING@: 01008 (Warning)
warning_implicit_zero_bit_padding :: ByteString
warning_implicit_zero_bit_padding = "01008"

-- |@WARNING_NULL_VALUE_ELIMINATED_IN_SET_FUNCTION@: 01003 (Warning)
warning_null_value_eliminated_in_set_function :: ByteString
warning_null_value_eliminated_in_set_function = "01003"

-- |@WARNING_PRIVILEGE_NOT_GRANTED@: 01007 (Warning)
warning_privilege_not_granted :: ByteString
warning_privilege_not_granted = "01007"

-- |@WARNING_PRIVILEGE_NOT_REVOKED@: 01006 (Warning)
warning_privilege_not_revoked :: ByteString
warning_privilege_not_revoked = "01006"

-- |@WARNING_STRING_DATA_RIGHT_TRUNCATION@: 01004 (Warning)
warning_string_data_right_truncation :: ByteString
warning_string_data_right_truncation = "01004"

-- |@WARNING_DEPRECATED_FEATURE@: 01P01 (Warning)
warning_deprecated_feature :: ByteString
warning_deprecated_feature = "01P01"

-- |@NO_DATA@: 02000 (Warning)
no_data :: ByteString
no_data = "02000"

-- |@NO_ADDITIONAL_DYNAMIC_RESULT_SETS_RETURNED@: 02001 (Warning)
no_additional_dynamic_result_sets_returned :: ByteString
no_additional_dynamic_result_sets_returned = "02001"

-- |@SQL_STATEMENT_NOT_YET_COMPLETE@: 03000 (Error)
sql_statement_not_yet_complete :: ByteString
sql_statement_not_yet_complete = "03000"

-- |@CONNECTION_EXCEPTION@: 08000 (Error)
connection_exception :: ByteString
connection_exception = "08000"

-- |@CONNECTION_DOES_NOT_EXIST@: 08003 (Error)
connection_does_not_exist :: ByteString
connection_does_not_exist = "08003"

-- |@CONNECTION_FAILURE@: 08006 (Error)
connection_failure :: ByteString
connection_failure = "08006"

-- |@SQLCLIENT_UNABLE_TO_ESTABLISH_SQLCONNECTION@: 08001 (Error)
sqlclient_unable_to_establish_sqlconnection :: ByteString
sqlclient_unable_to_establish_sqlconnection = "08001"

-- |@SQLSERVER_REJECTED_ESTABLISHMENT_OF_SQLCONNECTION@: 08004 (Error)
sqlserver_rejected_establishment_of_sqlconnection :: ByteString
sqlserver_rejected_establishment_of_sqlconnection = "08004"

-- |@TRANSACTION_RESOLUTION_UNKNOWN@: 08007 (Error)
transaction_resolution_unknown :: ByteString
transaction_resolution_unknown = "08007"

-- |@PROTOCOL_VIOLATION@: 08P01 (Error)
protocol_violation :: ByteString
protocol_violation = "08P01"

-- |@TRIGGERED_ACTION_EXCEPTION@: 09000 (Error)
triggered_action_exception :: ByteString
triggered_action_exception = "09000"

-- |@FEATURE_NOT_SUPPORTED@: 0A000 (Error)
feature_not_supported :: ByteString
feature_not_supported = "0A000"

-- |@INVALID_TRANSACTION_INITIATION@: 0B000 (Error)
invalid_transaction_initiation :: ByteString
invalid_transaction_initiation = "0B000"

-- |@LOCATOR_EXCEPTION@: 0F000 (Error)
locator_exception :: ByteString
locator_exception = "0F000"

-- |@L_E_INVALID_SPECIFICATION@: 0F001 (Error)
invalid_locator_specification :: ByteString
invalid_locator_specification = "0F001"

-- |@INVALID_GRANTOR@: 0L000 (Error)
invalid_grantor :: ByteString
invalid_grantor = "0L000"

-- |@INVALID_GRANT_OPERATION@: 0LP01 (Error)
invalid_grant_operation :: ByteString
invalid_grant_operation = "0LP01"

-- |@INVALID_ROLE_SPECIFICATION@: 0P000 (Error)
invalid_role_specification :: ByteString
invalid_role_specification = "0P000"

-- |@DIAGNOSTICS_EXCEPTION@: 0Z000 (Error)
diagnostics_exception :: ByteString
diagnostics_exception = "0Z000"

-- |@STACKED_DIAGNOSTICS_ACCESSED_WITHOUT_ACTIVE_HANDLER@: 0Z002 (Error)
stacked_diagnostics_accessed_without_active_handler :: ByteString
stacked_diagnostics_accessed_without_active_handler = "0Z002"

-- |@CASE_NOT_FOUND@: 20000 (Error)
case_not_found :: ByteString
case_not_found = "20000"

-- |@CARDINALITY_VIOLATION@: 21000 (Error)
cardinality_violation :: ByteString
cardinality_violation = "21000"

-- |@DATA_EXCEPTION@: 22000 (Error)
data_exception :: ByteString
data_exception = "22000"

-- |@ARRAY_ELEMENT_ERROR@: 2202E (Error)
_ARRAY_ELEMENT_ERROR :: ByteString
_ARRAY_ELEMENT_ERROR = "2202E"

-- |@ARRAY_SUBSCRIPT_ERROR@: 2202E (Error)
array_subscript_error :: ByteString
array_subscript_error = "2202E"

-- |@CHARACTER_NOT_IN_REPERTOIRE@: 22021 (Error)
character_not_in_repertoire :: ByteString
character_not_in_repertoire = "22021"

-- |@DATETIME_FIELD_OVERFLOW@: 22008 (Error)
datetime_field_overflow :: ByteString
datetime_field_overflow = "22008"

-- |@DATETIME_VALUE_OUT_OF_RANGE@: 22008 (Error)
_DATETIME_VALUE_OUT_OF_RANGE :: ByteString
_DATETIME_VALUE_OUT_OF_RANGE = "22008"

-- |@DIVISION_BY_ZERO@: 22012 (Error)
division_by_zero :: ByteString
division_by_zero = "22012"

-- |@ERROR_IN_ASSIGNMENT@: 22005 (Error)
error_in_assignment :: ByteString
error_in_assignment = "22005"

-- |@ESCAPE_CHARACTER_CONFLICT@: 2200B (Error)
escape_character_conflict :: ByteString
escape_character_conflict = "2200B"

-- |@INDICATOR_OVERFLOW@: 22022 (Error)
indicator_overflow :: ByteString
indicator_overflow = "22022"

-- |@INTERVAL_FIELD_OVERFLOW@: 22015 (Error)
interval_field_overflow :: ByteString
interval_field_overflow = "22015"

-- |@INVALID_ARGUMENT_FOR_LOG@: 2201E (Error)
invalid_argument_for_logarithm :: ByteString
invalid_argument_for_logarithm = "2201E"

-- |@INVALID_ARGUMENT_FOR_NTILE@: 22014 (Error)
invalid_argument_for_ntile_function :: ByteString
invalid_argument_for_ntile_function = "22014"

-- |@INVALID_ARGUMENT_FOR_NTH_VALUE@: 22016 (Error)
invalid_argument_for_nth_value_function :: ByteString
invalid_argument_for_nth_value_function = "22016"

-- |@INVALID_ARGUMENT_FOR_POWER_FUNCTION@: 2201F (Error)
invalid_argument_for_power_function :: ByteString
invalid_argument_for_power_function = "2201F"

-- |@INVALID_ARGUMENT_FOR_WIDTH_BUCKET_FUNCTION@: 2201G (Error)
invalid_argument_for_width_bucket_function :: ByteString
invalid_argument_for_width_bucket_function = "2201G"

-- |@INVALID_CHARACTER_VALUE_FOR_CAST@: 22018 (Error)
invalid_character_value_for_cast :: ByteString
invalid_character_value_for_cast = "22018"

-- |@INVALID_DATETIME_FORMAT@: 22007 (Error)
invalid_datetime_format :: ByteString
invalid_datetime_format = "22007"

-- |@INVALID_ESCAPE_CHARACTER@: 22019 (Error)
invalid_escape_character :: ByteString
invalid_escape_character = "22019"

-- |@INVALID_ESCAPE_OCTET@: 2200D (Error)
invalid_escape_octet :: ByteString
invalid_escape_octet = "2200D"

-- |@INVALID_ESCAPE_SEQUENCE@: 22025 (Error)
invalid_escape_sequence :: ByteString
invalid_escape_sequence = "22025"

-- |@NONSTANDARD_USE_OF_ESCAPE_CHARACTER@: 22P06 (Error)
nonstandard_use_of_escape_character :: ByteString
nonstandard_use_of_escape_character = "22P06"

-- |@INVALID_INDICATOR_PARAMETER_VALUE@: 22010 (Error)
invalid_indicator_parameter_value :: ByteString
invalid_indicator_parameter_value = "22010"

-- |@INVALID_PARAMETER_VALUE@: 22023 (Error)
invalid_parameter_value :: ByteString
invalid_parameter_value = "22023"

-- |@INVALID_PRECEDING_OR_FOLLOWING_SIZE@: 22013 (Error)
invalid_preceding_or_following_size :: ByteString
invalid_preceding_or_following_size = "22013"

-- |@INVALID_REGULAR_EXPRESSION@: 2201B (Error)
invalid_regular_expression :: ByteString
invalid_regular_expression = "2201B"

-- |@INVALID_ROW_COUNT_IN_LIMIT_CLAUSE@: 2201W (Error)
invalid_row_count_in_limit_clause :: ByteString
invalid_row_count_in_limit_clause = "2201W"

-- |@INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE@: 2201X (Error)
invalid_row_count_in_result_offset_clause :: ByteString
invalid_row_count_in_result_offset_clause = "2201X"

-- |@INVALID_TABLESAMPLE_ARGUMENT@: 2202H (Error)
invalid_tablesample_argument :: ByteString
invalid_tablesample_argument = "2202H"

-- |@INVALID_TABLESAMPLE_REPEAT@: 2202G (Error)
invalid_tablesample_repeat :: ByteString
invalid_tablesample_repeat = "2202G"

-- |@INVALID_TIME_ZONE_DISPLACEMENT_VALUE@: 22009 (Error)
invalid_time_zone_displacement_value :: ByteString
invalid_time_zone_displacement_value = "22009"

-- |@INVALID_USE_OF_ESCAPE_CHARACTER@: 2200C (Error)
invalid_use_of_escape_character :: ByteString
invalid_use_of_escape_character = "2200C"

-- |@MOST_SPECIFIC_TYPE_MISMATCH@: 2200G (Error)
most_specific_type_mismatch :: ByteString
most_specific_type_mismatch = "2200G"

-- |@NULL_VALUE_NOT_ALLOWED@: 22004 (Error)
null_value_not_allowed :: ByteString
null_value_not_allowed = "22004"

-- |@NULL_VALUE_NO_INDICATOR_PARAMETER@: 22002 (Error)
null_value_no_indicator_parameter :: ByteString
null_value_no_indicator_parameter = "22002"

-- |@NUMERIC_VALUE_OUT_OF_RANGE@: 22003 (Error)
numeric_value_out_of_range :: ByteString
numeric_value_out_of_range = "22003"

-- |@SEQUENCE_GENERATOR_LIMIT_EXCEEDED@: 2200H (Error)
sequence_generator_limit_exceeded :: ByteString
sequence_generator_limit_exceeded = "2200H"

-- |@STRING_DATA_LENGTH_MISMATCH@: 22026 (Error)
string_data_length_mismatch :: ByteString
string_data_length_mismatch = "22026"

-- |@STRING_DATA_RIGHT_TRUNCATION@: 22001 (Error)
string_data_right_truncation :: ByteString
string_data_right_truncation = "22001"

-- |@SUBSTRING_ERROR@: 22011 (Error)
substring_error :: ByteString
substring_error = "22011"

-- |@TRIM_ERROR@: 22027 (Error)
trim_error :: ByteString
trim_error = "22027"

-- |@UNTERMINATED_C_STRING@: 22024 (Error)
unterminated_c_string :: ByteString
unterminated_c_string = "22024"

-- |@ZERO_LENGTH_CHARACTER_STRING@: 2200F (Error)
zero_length_character_string :: ByteString
zero_length_character_string = "2200F"

-- |@FLOATING_POINT_EXCEPTION@: 22P01 (Error)
floating_point_exception :: ByteString
floating_point_exception = "22P01"

-- |@INVALID_TEXT_REPRESENTATION@: 22P02 (Error)
invalid_text_representation :: ByteString
invalid_text_representation = "22P02"

-- |@INVALID_BINARY_REPRESENTATION@: 22P03 (Error)
invalid_binary_representation :: ByteString
invalid_binary_representation = "22P03"

-- |@BAD_COPY_FILE_FORMAT@: 22P04 (Error)
bad_copy_file_format :: ByteString
bad_copy_file_format = "22P04"

-- |@UNTRANSLATABLE_CHARACTER@: 22P05 (Error)
untranslatable_character :: ByteString
untranslatable_character = "22P05"

-- |@NOT_AN_XML_DOCUMENT@: 2200L (Error)
not_an_xml_document :: ByteString
not_an_xml_document = "2200L"

-- |@INVALID_XML_DOCUMENT@: 2200M (Error)
invalid_xml_document :: ByteString
invalid_xml_document = "2200M"

-- |@INVALID_XML_CONTENT@: 2200N (Error)
invalid_xml_content :: ByteString
invalid_xml_content = "2200N"

-- |@INVALID_XML_COMMENT@: 2200S (Error)
invalid_xml_comment :: ByteString
invalid_xml_comment = "2200S"

-- |@INVALID_XML_PROCESSING_INSTRUCTION@: 2200T (Error)
invalid_xml_processing_instruction :: ByteString
invalid_xml_processing_instruction = "2200T"

-- |@DUPLICATE_JSON_OBJECT_KEY_VALUE@: 22030 (Error)
duplicate_json_object_key_value :: ByteString
duplicate_json_object_key_value = "22030"

-- |@INVALID_JSON_TEXT@: 22032 (Error)
invalid_json_text :: ByteString
invalid_json_text = "22032"

-- |@INVALID_SQL_JSON_SUBSCRIPT@: 22033 (Error)
invalid_sql_json_subscript :: ByteString
invalid_sql_json_subscript = "22033"

-- |@MORE_THAN_ONE_SQL_JSON_ITEM@: 22034 (Error)
more_than_one_sql_json_item :: ByteString
more_than_one_sql_json_item = "22034"

-- |@NO_SQL_JSON_ITEM@: 22035 (Error)
no_sql_json_item :: ByteString
no_sql_json_item = "22035"

-- |@NON_NUMERIC_SQL_JSON_ITEM@: 22036 (Error)
non_numeric_sql_json_item :: ByteString
non_numeric_sql_json_item = "22036"

-- |@NON_UNIQUE_KEYS_IN_A_JSON_OBJECT@: 22037 (Error)
non_unique_keys_in_a_json_object :: ByteString
non_unique_keys_in_a_json_object = "22037"

-- |@SINGLETON_SQL_JSON_ITEM_REQUIRED@: 22038 (Error)
singleton_sql_json_item_required :: ByteString
singleton_sql_json_item_required = "22038"

-- |@SQL_JSON_ARRAY_NOT_FOUND@: 22039 (Error)
sql_json_array_not_found :: ByteString
sql_json_array_not_found = "22039"

-- |@SQL_JSON_MEMBER_NOT_FOUND@: 2203A (Error)
sql_json_member_not_found :: ByteString
sql_json_member_not_found = "2203A"

-- |@SQL_JSON_NUMBER_NOT_FOUND@: 2203B (Error)
sql_json_number_not_found :: ByteString
sql_json_number_not_found = "2203B"

-- |@SQL_JSON_OBJECT_NOT_FOUND@: 2203C (Error)
sql_json_object_not_found :: ByteString
sql_json_object_not_found = "2203C"

-- |@TOO_MANY_JSON_ARRAY_ELEMENTS@: 2203D (Error)
too_many_json_array_elements :: ByteString
too_many_json_array_elements = "2203D"

-- |@TOO_MANY_JSON_OBJECT_MEMBERS@: 2203E (Error)
too_many_json_object_members :: ByteString
too_many_json_object_members = "2203E"

-- |@SQL_JSON_SCALAR_REQUIRED@: 2203F (Error)
sql_json_scalar_required :: ByteString
sql_json_scalar_required = "2203F"

-- |@INTEGRITY_CONSTRAINT_VIOLATION@: 23000 (Error)
integrity_constraint_violation :: ByteString
integrity_constraint_violation = "23000"

-- |@RESTRICT_VIOLATION@: 23001 (Error)
restrict_violation :: ByteString
restrict_violation = "23001"

-- |@NOT_NULL_VIOLATION@: 23502 (Error)
not_null_violation :: ByteString
not_null_violation = "23502"

-- |@FOREIGN_KEY_VIOLATION@: 23503 (Error)
foreign_key_violation :: ByteString
foreign_key_violation = "23503"

-- |@UNIQUE_VIOLATION@: 23505 (Error)
unique_violation :: ByteString
unique_violation = "23505"

-- |@CHECK_VIOLATION@: 23514 (Error)
check_violation :: ByteString
check_violation = "23514"

-- |@EXCLUSION_VIOLATION@: 23P01 (Error)
exclusion_violation :: ByteString
exclusion_violation = "23P01"

-- |@INVALID_CURSOR_STATE@: 24000 (Error)
invalid_cursor_state :: ByteString
invalid_cursor_state = "24000"

-- |@INVALID_TRANSACTION_STATE@: 25000 (Error)
invalid_transaction_state :: ByteString
invalid_transaction_state = "25000"

-- |@ACTIVE_SQL_TRANSACTION@: 25001 (Error)
active_sql_transaction :: ByteString
active_sql_transaction = "25001"

-- |@BRANCH_TRANSACTION_ALREADY_ACTIVE@: 25002 (Error)
branch_transaction_already_active :: ByteString
branch_transaction_already_active = "25002"

-- |@HELD_CURSOR_REQUIRES_SAME_ISOLATION_LEVEL@: 25008 (Error)
held_cursor_requires_same_isolation_level :: ByteString
held_cursor_requires_same_isolation_level = "25008"

-- |@INAPPROPRIATE_ACCESS_MODE_FOR_BRANCH_TRANSACTION@: 25003 (Error)
inappropriate_access_mode_for_branch_transaction :: ByteString
inappropriate_access_mode_for_branch_transaction = "25003"

-- |@INAPPROPRIATE_ISOLATION_LEVEL_FOR_BRANCH_TRANSACTION@: 25004 (Error)
inappropriate_isolation_level_for_branch_transaction :: ByteString
inappropriate_isolation_level_for_branch_transaction = "25004"

-- |@NO_ACTIVE_SQL_TRANSACTION_FOR_BRANCH_TRANSACTION@: 25005 (Error)
no_active_sql_transaction_for_branch_transaction :: ByteString
no_active_sql_transaction_for_branch_transaction = "25005"

-- |@READ_ONLY_SQL_TRANSACTION@: 25006 (Error)
read_only_sql_transaction :: ByteString
read_only_sql_transaction = "25006"

-- |@SCHEMA_AND_DATA_STATEMENT_MIXING_NOT_SUPPORTED@: 25007 (Error)
schema_and_data_statement_mixing_not_supported :: ByteString
schema_and_data_statement_mixing_not_supported = "25007"

-- |@NO_ACTIVE_SQL_TRANSACTION@: 25P01 (Error)
no_active_sql_transaction :: ByteString
no_active_sql_transaction = "25P01"

-- |@IN_FAILED_SQL_TRANSACTION@: 25P02 (Error)
in_failed_sql_transaction :: ByteString
in_failed_sql_transaction = "25P02"

-- |@IDLE_IN_TRANSACTION_SESSION_TIMEOUT@: 25P03 (Error)
idle_in_transaction_session_timeout :: ByteString
idle_in_transaction_session_timeout = "25P03"

-- |@INVALID_SQL_STATEMENT_NAME@: 26000 (Error)
invalid_sql_statement_name :: ByteString
invalid_sql_statement_name = "26000"

-- |@TRIGGERED_DATA_CHANGE_VIOLATION@: 27000 (Error)
triggered_data_change_violation :: ByteString
triggered_data_change_violation = "27000"

-- |@INVALID_AUTHORIZATION_SPECIFICATION@: 28000 (Error)
invalid_authorization_specification :: ByteString
invalid_authorization_specification = "28000"

-- |@INVALID_PASSWORD@: 28P01 (Error)
invalid_password :: ByteString
invalid_password = "28P01"

-- |@DEPENDENT_PRIVILEGE_DESCRIPTORS_STILL_EXIST@: 2B000 (Error)
dependent_privilege_descriptors_still_exist :: ByteString
dependent_privilege_descriptors_still_exist = "2B000"

-- |@DEPENDENT_OBJECTS_STILL_EXIST@: 2BP01 (Error)
dependent_objects_still_exist :: ByteString
dependent_objects_still_exist = "2BP01"

-- |@INVALID_TRANSACTION_TERMINATION@: 2D000 (Error)
invalid_transaction_termination :: ByteString
invalid_transaction_termination = "2D000"

-- |@SQL_ROUTINE_EXCEPTION@: 2F000 (Error)
sql_routine_exception :: ByteString
sql_routine_exception = "2F000"

-- |@S_R_E_FUNCTION_EXECUTED_NO_RETURN_STATEMENT@: 2F005 (Error)
s_r_e_function_executed_no_return_statement :: ByteString
s_r_e_function_executed_no_return_statement = "2F005"

-- |@S_R_E_MODIFYING_SQL_DATA_NOT_PERMITTED@: 2F002 (Error)
s_r_e_modifying_sql_data_not_permitted :: ByteString
s_r_e_modifying_sql_data_not_permitted = "2F002"

-- |@S_R_E_PROHIBITED_SQL_STATEMENT_ATTEMPTED@: 2F003 (Error)
s_r_e_prohibited_sql_statement_attempted :: ByteString
s_r_e_prohibited_sql_statement_attempted = "2F003"

-- |@S_R_E_READING_SQL_DATA_NOT_PERMITTED@: 2F004 (Error)
s_r_e_reading_sql_data_not_permitted :: ByteString
s_r_e_reading_sql_data_not_permitted = "2F004"

-- |@INVALID_CURSOR_NAME@: 34000 (Error)
invalid_cursor_name :: ByteString
invalid_cursor_name = "34000"

-- |@EXTERNAL_ROUTINE_EXCEPTION@: 38000 (Error)
external_routine_exception :: ByteString
external_routine_exception = "38000"

-- |@E_R_E_CONTAINING_SQL_NOT_PERMITTED@: 38001 (Error)
e_r_e_containing_sql_not_permitted :: ByteString
e_r_e_containing_sql_not_permitted = "38001"

-- |@E_R_E_MODIFYING_SQL_DATA_NOT_PERMITTED@: 38002 (Error)
e_r_e_modifying_sql_data_not_permitted :: ByteString
e_r_e_modifying_sql_data_not_permitted = "38002"

-- |@E_R_E_PROHIBITED_SQL_STATEMENT_ATTEMPTED@: 38003 (Error)
e_r_e_prohibited_sql_statement_attempted :: ByteString
e_r_e_prohibited_sql_statement_attempted = "38003"

-- |@E_R_E_READING_SQL_DATA_NOT_PERMITTED@: 38004 (Error)
e_r_e_reading_sql_data_not_permitted :: ByteString
e_r_e_reading_sql_data_not_permitted = "38004"

-- |@EXTERNAL_ROUTINE_INVOCATION_EXCEPTION@: 39000 (Error)
external_routine_invocation_exception :: ByteString
external_routine_invocation_exception = "39000"

-- |@E_R_I_E_INVALID_SQLSTATE_RETURNED@: 39001 (Error)
e_r_i_e_invalid_sqlstate_returned :: ByteString
e_r_i_e_invalid_sqlstate_returned = "39001"

-- |@E_R_I_E_NULL_VALUE_NOT_ALLOWED@: 39004 (Error)
e_r_i_e_null_value_not_allowed :: ByteString
e_r_i_e_null_value_not_allowed = "39004"

-- |@E_R_I_E_TRIGGER_PROTOCOL_VIOLATED@: 39P01 (Error)
e_r_i_e_trigger_protocol_violated :: ByteString
e_r_i_e_trigger_protocol_violated = "39P01"

-- |@E_R_I_E_SRF_PROTOCOL_VIOLATED@: 39P02 (Error)
e_r_i_e_srf_protocol_violated :: ByteString
e_r_i_e_srf_protocol_violated = "39P02"

-- |@E_R_I_E_EVENT_TRIGGER_PROTOCOL_VIOLATED@: 39P03 (Error)
e_r_i_e_event_trigger_protocol_violated :: ByteString
e_r_i_e_event_trigger_protocol_violated = "39P03"

-- |@SAVEPOINT_EXCEPTION@: 3B000 (Error)
savepoint_exception :: ByteString
savepoint_exception = "3B000"

-- |@S_E_INVALID_SPECIFICATION@: 3B001 (Error)
invalid_savepoint_specification :: ByteString
invalid_savepoint_specification = "3B001"

-- |@INVALID_CATALOG_NAME@: 3D000 (Error)
invalid_catalog_name :: ByteString
invalid_catalog_name = "3D000"

-- |@INVALID_SCHEMA_NAME@: 3F000 (Error)
invalid_schema_name :: ByteString
invalid_schema_name = "3F000"

-- |@TRANSACTION_ROLLBACK@: 40000 (Error)
transaction_rollback :: ByteString
transaction_rollback = "40000"

-- |@T_R_INTEGRITY_CONSTRAINT_VIOLATION@: 40002 (Error)
transaction_integrity_constraint_violation :: ByteString
transaction_integrity_constraint_violation = "40002"

-- |@T_R_SERIALIZATION_FAILURE@: 40001 (Error)
serialization_failure :: ByteString
serialization_failure = "40001"

-- |@T_R_STATEMENT_COMPLETION_UNKNOWN@: 40003 (Error)
statement_completion_unknown :: ByteString
statement_completion_unknown = "40003"

-- |@T_R_DEADLOCK_DETECTED@: 40P01 (Error)
deadlock_detected :: ByteString
deadlock_detected = "40P01"

-- |@SYNTAX_ERROR_OR_ACCESS_RULE_VIOLATION@: 42000 (Error)
syntax_error_or_access_rule_violation :: ByteString
syntax_error_or_access_rule_violation = "42000"

-- |@SYNTAX_ERROR@: 42601 (Error)
syntax_error :: ByteString
syntax_error = "42601"

-- |@INSUFFICIENT_PRIVILEGE@: 42501 (Error)
insufficient_privilege :: ByteString
insufficient_privilege = "42501"

-- |@CANNOT_COERCE@: 42846 (Error)
cannot_coerce :: ByteString
cannot_coerce = "42846"

-- |@GROUPING_ERROR@: 42803 (Error)
grouping_error :: ByteString
grouping_error = "42803"

-- |@WINDOWING_ERROR@: 42P20 (Error)
windowing_error :: ByteString
windowing_error = "42P20"

-- |@INVALID_RECURSION@: 42P19 (Error)
invalid_recursion :: ByteString
invalid_recursion = "42P19"

-- |@INVALID_FOREIGN_KEY@: 42830 (Error)
invalid_foreign_key :: ByteString
invalid_foreign_key = "42830"

-- |@INVALID_NAME@: 42602 (Error)
invalid_name :: ByteString
invalid_name = "42602"

-- |@NAME_TOO_LONG@: 42622 (Error)
name_too_long :: ByteString
name_too_long = "42622"

-- |@RESERVED_NAME@: 42939 (Error)
reserved_name :: ByteString
reserved_name = "42939"

-- |@DATATYPE_MISMATCH@: 42804 (Error)
datatype_mismatch :: ByteString
datatype_mismatch = "42804"

-- |@INDETERMINATE_DATATYPE@: 42P18 (Error)
indeterminate_datatype :: ByteString
indeterminate_datatype = "42P18"

-- |@COLLATION_MISMATCH@: 42P21 (Error)
collation_mismatch :: ByteString
collation_mismatch = "42P21"

-- |@INDETERMINATE_COLLATION@: 42P22 (Error)
indeterminate_collation :: ByteString
indeterminate_collation = "42P22"

-- |@WRONG_OBJECT_TYPE@: 42809 (Error)
wrong_object_type :: ByteString
wrong_object_type = "42809"

-- |@GENERATED_ALWAYS@: 428C9 (Error)
generated_always :: ByteString
generated_always = "428C9"

-- |@UNDEFINED_COLUMN@: 42703 (Error)
undefined_column :: ByteString
undefined_column = "42703"

-- |@UNDEFINED_CURSOR@: 34000 (Error)
_UNDEFINED_CURSOR :: ByteString
_UNDEFINED_CURSOR = "34000"

-- |@UNDEFINED_DATABASE@: 3D000 (Error)
_UNDEFINED_DATABASE :: ByteString
_UNDEFINED_DATABASE = "3D000"

-- |@UNDEFINED_FUNCTION@: 42883 (Error)
undefined_function :: ByteString
undefined_function = "42883"

-- |@UNDEFINED_PSTATEMENT@: 26000 (Error)
_UNDEFINED_PSTATEMENT :: ByteString
_UNDEFINED_PSTATEMENT = "26000"

-- |@UNDEFINED_SCHEMA@: 3F000 (Error)
_UNDEFINED_SCHEMA :: ByteString
_UNDEFINED_SCHEMA = "3F000"

-- |@UNDEFINED_TABLE@: 42P01 (Error)
undefined_table :: ByteString
undefined_table = "42P01"

-- |@UNDEFINED_PARAMETER@: 42P02 (Error)
undefined_parameter :: ByteString
undefined_parameter = "42P02"

-- |@UNDEFINED_OBJECT@: 42704 (Error)
undefined_object :: ByteString
undefined_object = "42704"

-- |@DUPLICATE_COLUMN@: 42701 (Error)
duplicate_column :: ByteString
duplicate_column = "42701"

-- |@DUPLICATE_CURSOR@: 42P03 (Error)
duplicate_cursor :: ByteString
duplicate_cursor = "42P03"

-- |@DUPLICATE_DATABASE@: 42P04 (Error)
duplicate_database :: ByteString
duplicate_database = "42P04"

-- |@DUPLICATE_FUNCTION@: 42723 (Error)
duplicate_function :: ByteString
duplicate_function = "42723"

-- |@DUPLICATE_PSTATEMENT@: 42P05 (Error)
duplicate_prepared_statement :: ByteString
duplicate_prepared_statement = "42P05"

-- |@DUPLICATE_SCHEMA@: 42P06 (Error)
duplicate_schema :: ByteString
duplicate_schema = "42P06"

-- |@DUPLICATE_TABLE@: 42P07 (Error)
duplicate_table :: ByteString
duplicate_table = "42P07"

-- |@DUPLICATE_ALIAS@: 42712 (Error)
duplicate_alias :: ByteString
duplicate_alias = "42712"

-- |@DUPLICATE_OBJECT@: 42710 (Error)
duplicate_object :: ByteString
duplicate_object = "42710"

-- |@AMBIGUOUS_COLUMN@: 42702 (Error)
ambiguous_column :: ByteString
ambiguous_column = "42702"

-- |@AMBIGUOUS_FUNCTION@: 42725 (Error)
ambiguous_function :: ByteString
ambiguous_function = "42725"

-- |@AMBIGUOUS_PARAMETER@: 42P08 (Error)
ambiguous_parameter :: ByteString
ambiguous_parameter = "42P08"

-- |@AMBIGUOUS_ALIAS@: 42P09 (Error)
ambiguous_alias :: ByteString
ambiguous_alias = "42P09"

-- |@INVALID_COLUMN_REFERENCE@: 42P10 (Error)
invalid_column_reference :: ByteString
invalid_column_reference = "42P10"

-- |@INVALID_COLUMN_DEFINITION@: 42611 (Error)
invalid_column_definition :: ByteString
invalid_column_definition = "42611"

-- |@INVALID_CURSOR_DEFINITION@: 42P11 (Error)
invalid_cursor_definition :: ByteString
invalid_cursor_definition = "42P11"

-- |@INVALID_DATABASE_DEFINITION@: 42P12 (Error)
invalid_database_definition :: ByteString
invalid_database_definition = "42P12"

-- |@INVALID_FUNCTION_DEFINITION@: 42P13 (Error)
invalid_function_definition :: ByteString
invalid_function_definition = "42P13"

-- |@INVALID_PSTATEMENT_DEFINITION@: 42P14 (Error)
invalid_prepared_statement_definition :: ByteString
invalid_prepared_statement_definition = "42P14"

-- |@INVALID_SCHEMA_DEFINITION@: 42P15 (Error)
invalid_schema_definition :: ByteString
invalid_schema_definition = "42P15"

-- |@INVALID_TABLE_DEFINITION@: 42P16 (Error)
invalid_table_definition :: ByteString
invalid_table_definition = "42P16"

-- |@INVALID_OBJECT_DEFINITION@: 42P17 (Error)
invalid_object_definition :: ByteString
invalid_object_definition = "42P17"

-- |@WITH_CHECK_OPTION_VIOLATION@: 44000 (Error)
with_check_option_violation :: ByteString
with_check_option_violation = "44000"

-- |@INSUFFICIENT_RESOURCES@: 53000 (Error)
insufficient_resources :: ByteString
insufficient_resources = "53000"

-- |@DISK_FULL@: 53100 (Error)
disk_full :: ByteString
disk_full = "53100"

-- |@OUT_OF_MEMORY@: 53200 (Error)
out_of_memory :: ByteString
out_of_memory = "53200"

-- |@TOO_MANY_CONNECTIONS@: 53300 (Error)
too_many_connections :: ByteString
too_many_connections = "53300"

-- |@CONFIGURATION_LIMIT_EXCEEDED@: 53400 (Error)
configuration_limit_exceeded :: ByteString
configuration_limit_exceeded = "53400"

-- |@PROGRAM_LIMIT_EXCEEDED@: 54000 (Error)
program_limit_exceeded :: ByteString
program_limit_exceeded = "54000"

-- |@STATEMENT_TOO_COMPLEX@: 54001 (Error)
statement_too_complex :: ByteString
statement_too_complex = "54001"

-- |@TOO_MANY_COLUMNS@: 54011 (Error)
too_many_columns :: ByteString
too_many_columns = "54011"

-- |@TOO_MANY_ARGUMENTS@: 54023 (Error)
too_many_arguments :: ByteString
too_many_arguments = "54023"

-- |@OBJECT_NOT_IN_PREREQUISITE_STATE@: 55000 (Error)
object_not_in_prerequisite_state :: ByteString
object_not_in_prerequisite_state = "55000"

-- |@OBJECT_IN_USE@: 55006 (Error)
object_in_use :: ByteString
object_in_use = "55006"

-- |@CANT_CHANGE_RUNTIME_PARAM@: 55P02 (Error)
cant_change_runtime_param :: ByteString
cant_change_runtime_param = "55P02"

-- |@LOCK_NOT_AVAILABLE@: 55P03 (Error)
lock_not_available :: ByteString
lock_not_available = "55P03"

-- |@UNSAFE_NEW_ENUM_VALUE_USAGE@: 55P04 (Error)
unsafe_new_enum_value_usage :: ByteString
unsafe_new_enum_value_usage = "55P04"

-- |@OPERATOR_INTERVENTION@: 57000 (Error)
operator_intervention :: ByteString
operator_intervention = "57000"

-- |@QUERY_CANCELED@: 57014 (Error)
query_canceled :: ByteString
query_canceled = "57014"

-- |@ADMIN_SHUTDOWN@: 57P01 (Error)
admin_shutdown :: ByteString
admin_shutdown = "57P01"

-- |@CRASH_SHUTDOWN@: 57P02 (Error)
crash_shutdown :: ByteString
crash_shutdown = "57P02"

-- |@CANNOT_CONNECT_NOW@: 57P03 (Error)
cannot_connect_now :: ByteString
cannot_connect_now = "57P03"

-- |@DATABASE_DROPPED@: 57P04 (Error)
database_dropped :: ByteString
database_dropped = "57P04"

-- |@SYSTEM_ERROR@: 58000 (Error)
system_error :: ByteString
system_error = "58000"

-- |@IO_ERROR@: 58030 (Error)
io_error :: ByteString
io_error = "58030"

-- |@UNDEFINED_FILE@: 58P01 (Error)
undefined_file :: ByteString
undefined_file = "58P01"

-- |@DUPLICATE_FILE@: 58P02 (Error)
duplicate_file :: ByteString
duplicate_file = "58P02"

-- |@SNAPSHOT_TOO_OLD@: 72000 (Error)
snapshot_too_old :: ByteString
snapshot_too_old = "72000"

-- |@CONFIG_FILE_ERROR@: F0000 (Error)
config_file_error :: ByteString
config_file_error = "F0000"

-- |@LOCK_FILE_EXISTS@: F0001 (Error)
lock_file_exists :: ByteString
lock_file_exists = "F0001"

-- |@FDW_ERROR@: HV000 (Error)
fdw_error :: ByteString
fdw_error = "HV000"

-- |@FDW_COLUMN_NAME_NOT_FOUND@: HV005 (Error)
fdw_column_name_not_found :: ByteString
fdw_column_name_not_found = "HV005"

-- |@FDW_DYNAMIC_PARAMETER_VALUE_NEEDED@: HV002 (Error)
fdw_dynamic_parameter_value_needed :: ByteString
fdw_dynamic_parameter_value_needed = "HV002"

-- |@FDW_FUNCTION_SEQUENCE_ERROR@: HV010 (Error)
fdw_function_sequence_error :: ByteString
fdw_function_sequence_error = "HV010"

-- |@FDW_INCONSISTENT_DESCRIPTOR_INFORMATION@: HV021 (Error)
fdw_inconsistent_descriptor_information :: ByteString
fdw_inconsistent_descriptor_information = "HV021"

-- |@FDW_INVALID_ATTRIBUTE_VALUE@: HV024 (Error)
fdw_invalid_attribute_value :: ByteString
fdw_invalid_attribute_value = "HV024"

-- |@FDW_INVALID_COLUMN_NAME@: HV007 (Error)
fdw_invalid_column_name :: ByteString
fdw_invalid_column_name = "HV007"

-- |@FDW_INVALID_COLUMN_NUMBER@: HV008 (Error)
fdw_invalid_column_number :: ByteString
fdw_invalid_column_number = "HV008"

-- |@FDW_INVALID_DATA_TYPE@: HV004 (Error)
fdw_invalid_data_type :: ByteString
fdw_invalid_data_type = "HV004"

-- |@FDW_INVALID_DATA_TYPE_DESCRIPTORS@: HV006 (Error)
fdw_invalid_data_type_descriptors :: ByteString
fdw_invalid_data_type_descriptors = "HV006"

-- |@FDW_INVALID_DESCRIPTOR_FIELD_IDENTIFIER@: HV091 (Error)
fdw_invalid_descriptor_field_identifier :: ByteString
fdw_invalid_descriptor_field_identifier = "HV091"

-- |@FDW_INVALID_HANDLE@: HV00B (Error)
fdw_invalid_handle :: ByteString
fdw_invalid_handle = "HV00B"

-- |@FDW_INVALID_OPTION_INDEX@: HV00C (Error)
fdw_invalid_option_index :: ByteString
fdw_invalid_option_index = "HV00C"

-- |@FDW_INVALID_OPTION_NAME@: HV00D (Error)
fdw_invalid_option_name :: ByteString
fdw_invalid_option_name = "HV00D"

-- |@FDW_INVALID_STRING_LENGTH_OR_BUFFER_LENGTH@: HV090 (Error)
fdw_invalid_string_length_or_buffer_length :: ByteString
fdw_invalid_string_length_or_buffer_length = "HV090"

-- |@FDW_INVALID_STRING_FORMAT@: HV00A (Error)
fdw_invalid_string_format :: ByteString
fdw_invalid_string_format = "HV00A"

-- |@FDW_INVALID_USE_OF_NULL_POINTER@: HV009 (Error)
fdw_invalid_use_of_null_pointer :: ByteString
fdw_invalid_use_of_null_pointer = "HV009"

-- |@FDW_TOO_MANY_HANDLES@: HV014 (Error)
fdw_too_many_handles :: ByteString
fdw_too_many_handles = "HV014"

-- |@FDW_OUT_OF_MEMORY@: HV001 (Error)
fdw_out_of_memory :: ByteString
fdw_out_of_memory = "HV001"

-- |@FDW_NO_SCHEMAS@: HV00P (Error)
fdw_no_schemas :: ByteString
fdw_no_schemas = "HV00P"

-- |@FDW_OPTION_NAME_NOT_FOUND@: HV00J (Error)
fdw_option_name_not_found :: ByteString
fdw_option_name_not_found = "HV00J"

-- |@FDW_REPLY_HANDLE@: HV00K (Error)
fdw_reply_handle :: ByteString
fdw_reply_handle = "HV00K"

-- |@FDW_SCHEMA_NOT_FOUND@: HV00Q (Error)
fdw_schema_not_found :: ByteString
fdw_schema_not_found = "HV00Q"

-- |@FDW_TABLE_NOT_FOUND@: HV00R (Error)
fdw_table_not_found :: ByteString
fdw_table_not_found = "HV00R"

-- |@FDW_UNABLE_TO_CREATE_EXECUTION@: HV00L (Error)
fdw_unable_to_create_execution :: ByteString
fdw_unable_to_create_execution = "HV00L"

-- |@FDW_UNABLE_TO_CREATE_REPLY@: HV00M (Error)
fdw_unable_to_create_reply :: ByteString
fdw_unable_to_create_reply = "HV00M"

-- |@FDW_UNABLE_TO_ESTABLISH_CONNECTION@: HV00N (Error)
fdw_unable_to_establish_connection :: ByteString
fdw_unable_to_establish_connection = "HV00N"

-- |@PLPGSQL_ERROR@: P0000 (Error)
plpgsql_error :: ByteString
plpgsql_error = "P0000"

-- |@RAISE_EXCEPTION@: P0001 (Error)
raise_exception :: ByteString
raise_exception = "P0001"

-- |@NO_DATA_FOUND@: P0002 (Error)
no_data_found :: ByteString
no_data_found = "P0002"

-- |@TOO_MANY_ROWS@: P0003 (Error)
too_many_rows :: ByteString
too_many_rows = "P0003"

-- |@ASSERT_FAILURE@: P0004 (Error)
assert_failure :: ByteString
assert_failure = "P0004"

-- |@INTERNAL_ERROR@: XX000 (Error)
internal_error :: ByteString
internal_error = "XX000"

-- |@DATA_CORRUPTED@: XX001 (Error)
data_corrupted :: ByteString
data_corrupted = "XX001"

-- |@INDEX_CORRUPTED@: XX002 (Error)
index_corrupted :: ByteString
index_corrupted = "XX002"

-- |All known error code names by code.
names :: Map ByteString String
names = fromDistinctAscList
  [(successful_completion,"successful_completion")
  ,(warning,"warning")
  ,(warning_null_value_eliminated_in_set_function,"null_value_eliminated_in_set_function")
  ,(warning_string_data_right_truncation,"string_data_right_truncation")
  ,(warning_privilege_not_revoked,"privilege_not_revoked")
  ,(warning_privilege_not_granted,"privilege_not_granted")
  ,(warning_implicit_zero_bit_padding,"implicit_zero_bit_padding")
  ,(warning_dynamic_result_sets_returned,"dynamic_result_sets_returned")
  ,(warning_deprecated_feature,"deprecated_feature")
  ,(no_data,"no_data")
  ,(no_additional_dynamic_result_sets_returned,"no_additional_dynamic_result_sets_returned")
  ,(sql_statement_not_yet_complete,"sql_statement_not_yet_complete")
  ,(connection_exception,"connection_exception")
  ,(sqlclient_unable_to_establish_sqlconnection,"sqlclient_unable_to_establish_sqlconnection")
  ,(connection_does_not_exist,"connection_does_not_exist")
  ,(sqlserver_rejected_establishment_of_sqlconnection,"sqlserver_rejected_establishment_of_sqlconnection")
  ,(connection_failure,"connection_failure")
  ,(transaction_resolution_unknown,"transaction_resolution_unknown")
  ,(protocol_violation,"protocol_violation")
  ,(triggered_action_exception,"triggered_action_exception")
  ,(feature_not_supported,"feature_not_supported")
  ,(invalid_transaction_initiation,"invalid_transaction_initiation")
  ,(locator_exception,"locator_exception")
  ,(invalid_locator_specification,"invalid_locator_specification")
  ,(invalid_grantor,"invalid_grantor")
  ,(invalid_grant_operation,"invalid_grant_operation")
  ,(invalid_role_specification,"invalid_role_specification")
  ,(diagnostics_exception,"diagnostics_exception")
  ,(stacked_diagnostics_accessed_without_active_handler,"stacked_diagnostics_accessed_without_active_handler")
  ,(case_not_found,"case_not_found")
  ,(cardinality_violation,"cardinality_violation")
  ,(data_exception,"data_exception")
  ,(string_data_right_truncation,"string_data_right_truncation")
  ,(null_value_no_indicator_parameter,"null_value_no_indicator_parameter")
  ,(numeric_value_out_of_range,"numeric_value_out_of_range")
  ,(null_value_not_allowed,"null_value_not_allowed")
  ,(error_in_assignment,"error_in_assignment")
  ,(invalid_datetime_format,"invalid_datetime_format")
  ,(datetime_field_overflow,"datetime_field_overflow")
  ,(_DATETIME_VALUE_OUT_OF_RANGE,"DATETIME_VALUE_OUT_OF_RANGE")
  ,(invalid_time_zone_displacement_value,"invalid_time_zone_displacement_value")
  ,(escape_character_conflict,"escape_character_conflict")
  ,(invalid_use_of_escape_character,"invalid_use_of_escape_character")
  ,(invalid_escape_octet,"invalid_escape_octet")
  ,(zero_length_character_string,"zero_length_character_string")
  ,(most_specific_type_mismatch,"most_specific_type_mismatch")
  ,(sequence_generator_limit_exceeded,"sequence_generator_limit_exceeded")
  ,(not_an_xml_document,"not_an_xml_document")
  ,(invalid_xml_document,"invalid_xml_document")
  ,(invalid_xml_content,"invalid_xml_content")
  ,(invalid_xml_comment,"invalid_xml_comment")
  ,(invalid_xml_processing_instruction,"invalid_xml_processing_instruction")
  ,(invalid_indicator_parameter_value,"invalid_indicator_parameter_value")
  ,(substring_error,"substring_error")
  ,(division_by_zero,"division_by_zero")
  ,(invalid_preceding_or_following_size,"invalid_preceding_or_following_size")
  ,(invalid_argument_for_ntile_function,"invalid_argument_for_ntile_function")
  ,(interval_field_overflow,"interval_field_overflow")
  ,(invalid_argument_for_nth_value_function,"invalid_argument_for_nth_value_function")
  ,(invalid_character_value_for_cast,"invalid_character_value_for_cast")
  ,(invalid_escape_character,"invalid_escape_character")
  ,(invalid_regular_expression,"invalid_regular_expression")
  ,(invalid_argument_for_logarithm,"invalid_argument_for_logarithm")
  ,(invalid_argument_for_power_function,"invalid_argument_for_power_function")
  ,(invalid_argument_for_width_bucket_function,"invalid_argument_for_width_bucket_function")
  ,(invalid_row_count_in_limit_clause,"invalid_row_count_in_limit_clause")
  ,(invalid_row_count_in_result_offset_clause,"invalid_row_count_in_result_offset_clause")
  ,(character_not_in_repertoire,"character_not_in_repertoire")
  ,(indicator_overflow,"indicator_overflow")
  ,(invalid_parameter_value,"invalid_parameter_value")
  ,(unterminated_c_string,"unterminated_c_string")
  ,(invalid_escape_sequence,"invalid_escape_sequence")
  ,(string_data_length_mismatch,"string_data_length_mismatch")
  ,(trim_error,"trim_error")
  ,(_ARRAY_ELEMENT_ERROR,"ARRAY_ELEMENT_ERROR")
  ,(array_subscript_error,"array_subscript_error")
  ,(invalid_tablesample_repeat,"invalid_tablesample_repeat")
  ,(invalid_tablesample_argument,"invalid_tablesample_argument")
  ,(duplicate_json_object_key_value,"duplicate_json_object_key_value")
  ,(invalid_json_text,"invalid_json_text")
  ,(invalid_sql_json_subscript,"invalid_sql_json_subscript")
  ,(more_than_one_sql_json_item,"more_than_one_sql_json_item")
  ,(no_sql_json_item,"no_sql_json_item")
  ,(non_numeric_sql_json_item,"non_numeric_sql_json_item")
  ,(non_unique_keys_in_a_json_object,"non_unique_keys_in_a_json_object")
  ,(singleton_sql_json_item_required,"singleton_sql_json_item_required")
  ,(sql_json_array_not_found,"sql_json_array_not_found")
  ,(sql_json_member_not_found,"sql_json_member_not_found")
  ,(sql_json_number_not_found,"sql_json_number_not_found")
  ,(sql_json_object_not_found,"sql_json_object_not_found")
  ,(too_many_json_array_elements,"too_many_json_array_elements")
  ,(too_many_json_object_members,"too_many_json_object_members")
  ,(sql_json_scalar_required,"sql_json_scalar_required")
  ,(floating_point_exception,"floating_point_exception")
  ,(invalid_text_representation,"invalid_text_representation")
  ,(invalid_binary_representation,"invalid_binary_representation")
  ,(bad_copy_file_format,"bad_copy_file_format")
  ,(untranslatable_character,"untranslatable_character")
  ,(nonstandard_use_of_escape_character,"nonstandard_use_of_escape_character")
  ,(integrity_constraint_violation,"integrity_constraint_violation")
  ,(restrict_violation,"restrict_violation")
  ,(not_null_violation,"not_null_violation")
  ,(foreign_key_violation,"foreign_key_violation")
  ,(unique_violation,"unique_violation")
  ,(check_violation,"check_violation")
  ,(exclusion_violation,"exclusion_violation")
  ,(invalid_cursor_state,"invalid_cursor_state")
  ,(invalid_transaction_state,"invalid_transaction_state")
  ,(active_sql_transaction,"active_sql_transaction")
  ,(branch_transaction_already_active,"branch_transaction_already_active")
  ,(inappropriate_access_mode_for_branch_transaction,"inappropriate_access_mode_for_branch_transaction")
  ,(inappropriate_isolation_level_for_branch_transaction,"inappropriate_isolation_level_for_branch_transaction")
  ,(no_active_sql_transaction_for_branch_transaction,"no_active_sql_transaction_for_branch_transaction")
  ,(read_only_sql_transaction,"read_only_sql_transaction")
  ,(schema_and_data_statement_mixing_not_supported,"schema_and_data_statement_mixing_not_supported")
  ,(held_cursor_requires_same_isolation_level,"held_cursor_requires_same_isolation_level")
  ,(no_active_sql_transaction,"no_active_sql_transaction")
  ,(in_failed_sql_transaction,"in_failed_sql_transaction")
  ,(idle_in_transaction_session_timeout,"idle_in_transaction_session_timeout")
  ,(invalid_sql_statement_name,"invalid_sql_statement_name")
  ,(_UNDEFINED_PSTATEMENT,"UNDEFINED_PSTATEMENT")
  ,(triggered_data_change_violation,"triggered_data_change_violation")
  ,(invalid_authorization_specification,"invalid_authorization_specification")
  ,(invalid_password,"invalid_password")
  ,(dependent_privilege_descriptors_still_exist,"dependent_privilege_descriptors_still_exist")
  ,(dependent_objects_still_exist,"dependent_objects_still_exist")
  ,(invalid_transaction_termination,"invalid_transaction_termination")
  ,(sql_routine_exception,"sql_routine_exception")
  ,(s_r_e_modifying_sql_data_not_permitted,"modifying_sql_data_not_permitted")
  ,(s_r_e_prohibited_sql_statement_attempted,"prohibited_sql_statement_attempted")
  ,(s_r_e_reading_sql_data_not_permitted,"reading_sql_data_not_permitted")
  ,(s_r_e_function_executed_no_return_statement,"function_executed_no_return_statement")
  ,(invalid_cursor_name,"invalid_cursor_name")
  ,(_UNDEFINED_CURSOR,"UNDEFINED_CURSOR")
  ,(external_routine_exception,"external_routine_exception")
  ,(e_r_e_containing_sql_not_permitted,"containing_sql_not_permitted")
  ,(e_r_e_modifying_sql_data_not_permitted,"modifying_sql_data_not_permitted")
  ,(e_r_e_prohibited_sql_statement_attempted,"prohibited_sql_statement_attempted")
  ,(e_r_e_reading_sql_data_not_permitted,"reading_sql_data_not_permitted")
  ,(external_routine_invocation_exception,"external_routine_invocation_exception")
  ,(e_r_i_e_invalid_sqlstate_returned,"invalid_sqlstate_returned")
  ,(e_r_i_e_null_value_not_allowed,"null_value_not_allowed")
  ,(e_r_i_e_trigger_protocol_violated,"trigger_protocol_violated")
  ,(e_r_i_e_srf_protocol_violated,"srf_protocol_violated")
  ,(e_r_i_e_event_trigger_protocol_violated,"event_trigger_protocol_violated")
  ,(savepoint_exception,"savepoint_exception")
  ,(invalid_savepoint_specification,"invalid_savepoint_specification")
  ,(invalid_catalog_name,"invalid_catalog_name")
  ,(_UNDEFINED_DATABASE,"UNDEFINED_DATABASE")
  ,(invalid_schema_name,"invalid_schema_name")
  ,(_UNDEFINED_SCHEMA,"UNDEFINED_SCHEMA")
  ,(transaction_rollback,"transaction_rollback")
  ,(serialization_failure,"serialization_failure")
  ,(transaction_integrity_constraint_violation,"transaction_integrity_constraint_violation")
  ,(statement_completion_unknown,"statement_completion_unknown")
  ,(deadlock_detected,"deadlock_detected")
  ,(syntax_error_or_access_rule_violation,"syntax_error_or_access_rule_violation")
  ,(insufficient_privilege,"insufficient_privilege")
  ,(syntax_error,"syntax_error")
  ,(invalid_name,"invalid_name")
  ,(invalid_column_definition,"invalid_column_definition")
  ,(name_too_long,"name_too_long")
  ,(duplicate_column,"duplicate_column")
  ,(ambiguous_column,"ambiguous_column")
  ,(undefined_column,"undefined_column")
  ,(undefined_object,"undefined_object")
  ,(duplicate_object,"duplicate_object")
  ,(duplicate_alias,"duplicate_alias")
  ,(duplicate_function,"duplicate_function")
  ,(ambiguous_function,"ambiguous_function")
  ,(grouping_error,"grouping_error")
  ,(datatype_mismatch,"datatype_mismatch")
  ,(wrong_object_type,"wrong_object_type")
  ,(invalid_foreign_key,"invalid_foreign_key")
  ,(cannot_coerce,"cannot_coerce")
  ,(undefined_function,"undefined_function")
  ,(generated_always,"generated_always")
  ,(reserved_name,"reserved_name")
  ,(undefined_table,"undefined_table")
  ,(undefined_parameter,"undefined_parameter")
  ,(duplicate_cursor,"duplicate_cursor")
  ,(duplicate_database,"duplicate_database")
  ,(duplicate_prepared_statement,"duplicate_prepared_statement")
  ,(duplicate_schema,"duplicate_schema")
  ,(duplicate_table,"duplicate_table")
  ,(ambiguous_parameter,"ambiguous_parameter")
  ,(ambiguous_alias,"ambiguous_alias")
  ,(invalid_column_reference,"invalid_column_reference")
  ,(invalid_cursor_definition,"invalid_cursor_definition")
  ,(invalid_database_definition,"invalid_database_definition")
  ,(invalid_function_definition,"invalid_function_definition")
  ,(invalid_prepared_statement_definition,"invalid_prepared_statement_definition")
  ,(invalid_schema_definition,"invalid_schema_definition")
  ,(invalid_table_definition,"invalid_table_definition")
  ,(invalid_object_definition,"invalid_object_definition")
  ,(indeterminate_datatype,"indeterminate_datatype")
  ,(invalid_recursion,"invalid_recursion")
  ,(windowing_error,"windowing_error")
  ,(collation_mismatch,"collation_mismatch")
  ,(indeterminate_collation,"indeterminate_collation")
  ,(with_check_option_violation,"with_check_option_violation")
  ,(insufficient_resources,"insufficient_resources")
  ,(disk_full,"disk_full")
  ,(out_of_memory,"out_of_memory")
  ,(too_many_connections,"too_many_connections")
  ,(configuration_limit_exceeded,"configuration_limit_exceeded")
  ,(program_limit_exceeded,"program_limit_exceeded")
  ,(statement_too_complex,"statement_too_complex")
  ,(too_many_columns,"too_many_columns")
  ,(too_many_arguments,"too_many_arguments")
  ,(object_not_in_prerequisite_state,"object_not_in_prerequisite_state")
  ,(object_in_use,"object_in_use")
  ,(cant_change_runtime_param,"cant_change_runtime_param")
  ,(lock_not_available,"lock_not_available")
  ,(unsafe_new_enum_value_usage,"unsafe_new_enum_value_usage")
  ,(operator_intervention,"operator_intervention")
  ,(query_canceled,"query_canceled")
  ,(admin_shutdown,"admin_shutdown")
  ,(crash_shutdown,"crash_shutdown")
  ,(cannot_connect_now,"cannot_connect_now")
  ,(database_dropped,"database_dropped")
  ,(system_error,"system_error")
  ,(io_error,"io_error")
  ,(undefined_file,"undefined_file")
  ,(duplicate_file,"duplicate_file")
  ,(snapshot_too_old,"snapshot_too_old")
  ,(config_file_error,"config_file_error")
  ,(lock_file_exists,"lock_file_exists")
  ,(fdw_error,"fdw_error")
  ,(fdw_out_of_memory,"fdw_out_of_memory")
  ,(fdw_dynamic_parameter_value_needed,"fdw_dynamic_parameter_value_needed")
  ,(fdw_invalid_data_type,"fdw_invalid_data_type")
  ,(fdw_column_name_not_found,"fdw_column_name_not_found")
  ,(fdw_invalid_data_type_descriptors,"fdw_invalid_data_type_descriptors")
  ,(fdw_invalid_column_name,"fdw_invalid_column_name")
  ,(fdw_invalid_column_number,"fdw_invalid_column_number")
  ,(fdw_invalid_use_of_null_pointer,"fdw_invalid_use_of_null_pointer")
  ,(fdw_invalid_string_format,"fdw_invalid_string_format")
  ,(fdw_invalid_handle,"fdw_invalid_handle")
  ,(fdw_invalid_option_index,"fdw_invalid_option_index")
  ,(fdw_invalid_option_name,"fdw_invalid_option_name")
  ,(fdw_option_name_not_found,"fdw_option_name_not_found")
  ,(fdw_reply_handle,"fdw_reply_handle")
  ,(fdw_unable_to_create_execution,"fdw_unable_to_create_execution")
  ,(fdw_unable_to_create_reply,"fdw_unable_to_create_reply")
  ,(fdw_unable_to_establish_connection,"fdw_unable_to_establish_connection")
  ,(fdw_no_schemas,"fdw_no_schemas")
  ,(fdw_schema_not_found,"fdw_schema_not_found")
  ,(fdw_table_not_found,"fdw_table_not_found")
  ,(fdw_function_sequence_error,"fdw_function_sequence_error")
  ,(fdw_too_many_handles,"fdw_too_many_handles")
  ,(fdw_inconsistent_descriptor_information,"fdw_inconsistent_descriptor_information")
  ,(fdw_invalid_attribute_value,"fdw_invalid_attribute_value")
  ,(fdw_invalid_string_length_or_buffer_length,"fdw_invalid_string_length_or_buffer_length")
  ,(fdw_invalid_descriptor_field_identifier,"fdw_invalid_descriptor_field_identifier")
  ,(plpgsql_error,"plpgsql_error")
  ,(raise_exception,"raise_exception")
  ,(no_data_found,"no_data_found")
  ,(too_many_rows,"too_many_rows")
  ,(assert_failure,"assert_failure")
  ,(internal_error,"internal_error")
  ,(data_corrupted,"data_corrupted")
  ,(index_corrupted,"index_corrupted")]
