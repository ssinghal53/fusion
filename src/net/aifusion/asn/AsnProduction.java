/**
 * Copyright 2018 Sharad Singhal, All Rights Reserved
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *    
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * Created Apr 15, 2018 by Sharad Singhal
 */
package net.aifusion.asn;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Enumeration to manage Productions known to the AstParser
 * @author Sharad Singhal
 */
public enum AsnProduction {
	// AST(production, isBuiltInType)
	ABSOLUTE_REFERENCE("AbsoluteReference",false),
	ACTUAL_PARAMETER("ActualParameter",false),
	ACTUAL_PARAMETER_LIST("ActualParameterList",false),
	ADDITIONAL_ELEMENT_SET_SPEC("AdditionalElementSetSpec",false),
	ADDITIONAL_ENUMERATION("AdditionalEnumeration",false),
	ALTERNATIVE_TYPE_LIST("AlternativeTypeList",false),
	ALTERNATIVE_TYPE_LISTS("AlternativeTypeLists",false),
	ANY_TYPE("OpenType",true),
	ASSIGNED_IDENTIFIER("AssignedIdentifier",false),
	ASSIGNMENT("Assignment",false),
	ASSIGNMENT_LIST("AssignmentList",false),
	AT_NOTATION("AtNotation",false),
	BIT_STRING_TYPE("BitStringType",true),
	BIT_STRING_VALUE("BitStringValue",false),
	BMP_STRING_TYPE("BMPString",true),
	BOOLEAN_TYPE("BooleanType",true),
	BOOLEAN_VALUE("BooleanValue",false),
	BUILTIN_TYPE("BuiltinType",false),
	BUILTIN_VALUE("BuiltinValue",false),
	CELL("Cell",false),
	CHAR_SYMS("CharSyms",false),
	CHARACTER_STRING_LIST("CharacterStringList",false),
	CHARACTER_STRING_TYPE("CharacterStringType",true),
	CHARACTER_STRING_VALUE("CharacterStringValue",false),
	CHARS_DEFN("CharsDefn",false),
	CHOICE_TYPE("ChoiceType",true),
	CHOICE_VALUE("ChoiceValue",false),
	CLASS_NUMBER("ClassNumber",false),
	COMPONENT_CONSTRAINT("ComponentConstraint",false),
	COMPONENT_ID("ComponentId",false),
	COMPONENT_ID_LIST("ComponentIdList",false),
	COMPONENT_RELATION_CONSTRAINT("ComponentRelationConstraint",false),
	COMPONENT_TYPE("ComponentType",false),
	COMPONENT_TYPE_LIST("ComponentTypeList",false),
	COMPONENT_TYPE_LISTS("ComponentTypeLists",false),
	COMPONENT_VALUE_LIST("ComponentValueList",false),
	CONSTRAINED_TYPE("ConstrainedType",false),
	CONSTRAINT("Constraint",false),
	CONSTRAINT_SPEC("ConstraintSpec",false),
	CONTAINED_SUBTYPE("ContainedSubtype",false),
	CONTENTS_CONSTRAINT("ContentsConstraint",false),
	DEFAULT_SYNTAX("DefaultSyntax",false),
	DEFINED_BY("DefinedBy",false),
	DEFINED_OBJECT("DefinedObject",false),
	DEFINED_OBJECT_CLASS("DefinedObjectClass",false),
	DEFINED_OBJECT_SET("DefinedObjectSet",false),
	DEFINED_SYNTAX("DefinedSyntax",false),
	DEFINED_SYNTAX_TOKEN("DefinedSyntaxToken",false),
	DEFINED_TYPE("DefinedType",false),
	DEFINED_VALUE("DefinedValue",false),
	DEFINITIVE_IDENTIFIER("DefinitiveIdentifier",false),
	DEFINITIVE_NAME_AND_NUMBER_FORM("DefinitiveNameAndNumberForm",false),
	DEFINITIVE_NUMBER_FORM("DefinitiveNumberForm",false),
	DEFINITIVE_OBJ_ID_COMPONENT("DefinitiveObjIdComponent",false),
	DEFINITIVE_OBJ_ID_COMPONENT_LIST("DefinitiveObjIdComponentList",false),
	DUMMY_GOVERNOR("DummyGovernor",false),
	DUMMY_REFERENCE("DummyReference",false),
	ELEMENT_SET_SPEC("ElementSetSpec",false),
	ELEMENT_SET_SPECS("ElementSetSpecs",false),
	ELEMENTS("Elements",false),
	ELEMS("Elems",false),
	EMBEDDED_PDV_TYPE("EmbeddedPDVType",true),
	EMBEDDED_PDV_VALUE("EmbeddedPDVValue",false),
	ENUMERATED_TYPE("EnumeratedType",true),
	ENUMERATED_VALUE("EnumeratedValue",false),
	ENUMERATION("Enumeration",false),
	ENUMERATION_ITEM("EnumerationItem",false),
	ENUMERATIONS("Enumerations",false),
	EXCEPTION_IDENTIFICATION("ExceptionIdentification",false),
	EXCEPTION_SPEC("ExceptionSpec",false),
	EXCLUSIONS("Exclusions",false),
	EXPORTS("Exports",false),
	EXTENSION("Extension",false),
	EXTENSION_ADDITION("ExtensionAddition",false),
	EXTENSION_ADDITION_ALTERNATIVE("ExtensionAdditionAlternative",false),
	EXTENSION_ADDITION_ALTERNATIVES("ExtensionAdditionAlternatives",false),
	EXTENSION_ADDITION_ALTERNATIVES_GROUP("ExtensionAdditionAlternativesGroup",false),
	EXTENSION_ADDITION_ALTERNATIVES_LIST("ExtensionAdditionAlternativesList",false),
	EXTENSION_ADDITION_GROUP("ExtensionAdditionGroup",false),
	EXTENSION_ADDITION_LIST("ExtensionAdditionList",false),
	EXTENSION_ADDITIONS("ExtensionAdditions",false),
	EXTENSION_AND_EXCEPTION("ExtensionAndException",false),
	EXTENSION_DEFAULT("ExtensionDefault",false),
	EXTENSION_END_MARKER("ExtensionEndMarker",false),
	EXTERNAL_OBJECT_CLASS_REFERENCE("ExternalObjectClassReference",false),
	EXTERNAL_OBJECT_REFERENCE("ExternalObjectReference",false),
	EXTERNAL_OBJECT_SET_REFERENCE("ExternalObjectSetReference",false),
	EXTERNAL_TYPE("ExternalType",true),
	EXTERNAL_TYPE_REFERENCE("ExternalTypeReference",false),
	EXTERNAL_VALUE("ExternalValue",false),
	EXTERNAL_VALUE_REFERENCE("ExternalValueReference",false),
	FALSE_TYPE("falseType",true),
	FALSE_VALUE("falseValue",false),
	FIELD_NAME("FieldName",false),
	FIELD_SETTING("FieldSetting",false),
	FIELD_SPEC("FieldSpec",false),
	FIXED_TYPE_FIELD_VAL("FixedTypeFieldVal",false),
	FIXED_TYPE_VALUE_FIELD_SPEC("FixedTypeValueFieldSpec",false),
	FIXED_TYPE_VALUE_SET_FIELD_SPEC("FixedTypeValueSetFieldSpec",false),
	FULL_SPECIFICATION("FullSpecification",false),
	GENERAL_CONSTRAINT("GeneralConstraint",false),
	GENERAL_STRING_TYPE("GeneralString",true),
	GENERALIZED_TIME_TYPE("GeneralizedTime",true),
	GLOBAL_MODULE_REFERENCE("GlobalModuleReference",false),
	GOVERNOR("Governor",false),
	GRAPHIC_STRING_TYPE("GraphicString",true),
	GROUP("Group",false),
	I_ELEMS("IElems",false),
	IA5_STRING_TYPE("IA5String",true),
	IDENTIFIER_LIST("IdentifierList",false),
	IMPORTS("Imports",false),
	INCLUDES("Includes",false),
	INFORMATION_FROM_OBJECTS("InformationFromObjects",false),
	INNER_TYPE_CONSTRAINTS("InnerTypeConstraints",false),
	INSTANCE_OF_TYPE("InstanceOfType",false),
	INSTANCE_OF_VALUE("InstanceOfValue",false),
	INTEGER_TYPE("IntegerType",true),
	INTEGER_VALUE("IntegerValue",false),
	INTERSECTION_ELEMENTS("IntersectionElements",false),
	INTERSECTION_MARK("IntersectionMark",false),
	INTERSECTIONS("Intersections",false),
	ISO646_STRING_TYPE("ISO646String",true),
	ITEM_ID("ItemId",false),
	ITEM_SPEC("ItemSpec",false),
	LEVEL("Level",false),
	LITERAL("Literal",false),
	LOWER_END_VALUE("LowerEndValue",false),
	LOWER_ENDPOINT("LowerEndpoint",false),
	MODULE_BODY("ModuleBody",false),
	MODULE_DEFINITION("ModuleDefinition",false),
	MODULE_IDENTIFIER("ModuleIdentifier",false),
	MULTIPLE_TYPE_CONSTRAINTS("MultipleTypeConstraints",false),
	NAME_AND_NUMBER_FORM("NameAndNumberForm",false),
	NAME_FORM("NameForm",false),
	NAMED_BIT("NamedBit",false),
	NAMED_BIT_LIST("NamedBitList",false),
	NAMED_CONSTRAINT("NamedConstraint",false),
	NAMED_NUMBER("NamedNumber",false),
	NAMED_NUMBER_LIST("NamedNumberList",false),
	NAMED_TYPE("NamedType",false),
	NAMED_VALUE("NamedValue",false),
	NON_PARAMETERIZED_TYPE_NAME("NonParameterizedTypeName",false),
	NULL_TYPE("NullType",true),
	NULL_VALUE("NullValue",false),
	NUMBER_FORM("NumberForm",false),
	NUMERIC_REAL_VALUE("NumericRealValue",false),
	NUMERIC_STRING_TYPE("NumericString",true),
	OBJ_ID_COMPONENTS("ObjIdComponents",false),
	OBJ_ID_COMPONENTS_LIST("ObjIdComponentsList",false),
	OBJECT("Object",false),
	OBJECT_ASSIGNMENT("ObjectAssignment",false),
	OBJECT_CLASS("ObjectClass",false),
	OBJECT_CLASS_ASSIGNMENT("ObjectClassAssignment",false),
	OBJECT_CLASS_DEFN("ObjectClassDefn",false),
	OBJECT_CLASS_FIELD_TYPE("ObjectClassFieldType",false),
	OBJECT_CLASS_FIELD_VALUE("ObjectClassFieldValue",false),
	OBJECT_CLASS_REFERENCE("objectclassreference",false),
	OBJECT_DEFN("ObjectDefn",false),
	OBJECT_DESCRIPTOR("ObjectDescriptor",true),
	// OBJECT_DESCRIPTOR_TYPE("ObjectDescriptorType",false),
	OBJECT_FIELD_REFERENCE("ObjectFieldReference",false),
	OBJECT_FIELD_SPEC("ObjectFieldSpec",false),
	OBJECT_FROM_OBJECT("ObjectFromObject",false),
	OBJECT_IDENTIFIER_TYPE("ObjectIdentifierType",true),
	OBJECT_IDENTIFIER_VALUE("ObjectIdentifierValue",false),
	OBJECT_OPTIONALITY_SPEC("ObjectOptionalitySpec",false),
	OBJECT_REFERENCE("objectreference",false),
	OBJECT_SET("ObjectSet",false),
	OBJECT_SET_ASSIGNMENT("ObjectSetAssignment",false),
	OBJECT_SET_ELEMENTS("ObjectSetElements",false),
	OBJECT_SET_FIELD_REFERENCE("ObjectSetFieldReference",false),
	OBJECT_SET_FIELD_SPEC("ObjectSetFieldSpec",false),
	OBJECT_SET_FROM_OBJECTS("ObjectSetFromObjects",false),
	OBJECT_SET_OPTIONALITY_SPEC("ObjectSetOptionalitySpec",false),
	OBJECT_SET_REFERENCE("objectsetreference",false),
	OBJECT_SET_SPEC("ObjectSetSpec",false),
	OCTET_STRING_TYPE("OctetStringType",true),
	OCTET_STRING_VALUE("OctetStringValue",false),
	OPEN_TYPE_FIELD_VAL("OpenTypeFieldVal",false),
	OPTIONAL_EXTENSION_MARKER("OptionalExtensionMarker",false),
	OPTIONAL_GROUP("OptionalGroup",false),
	PARAM_GOVERNOR("ParamGovernor",false),
	PARAMETER("Parameter",false),
	PARAMETER_LIST("ParameterList",false),
	PARAMETERIZED_ASSIGNMENT("ParameterizedAssignment",false),
	PARAMETERIZED_OBJECT("ParameterizedObject",false),
	PARAMETERIZED_OBJECT_ASSIGNMENT("ParameterizedObjectAssignment",false),
	PARAMETERIZED_OBJECT_CLASS("ParameterizedObjectClass",false),
	PARAMETERIZED_OBJECT_CLASS_ASSIGNMENT("ParameterizedObjectClassAssignment",false),
	PARAMETERIZED_OBJECT_SET("ParameterizedObjectSet",false),
	PARAMETERIZED_OBJECT_SET_ASSIGNMENT("ParameterizedObjectSetAssignment",false),
	PARAMETERIZED_REFERENCE("ParameterizedReference",false),
	PARAMETERIZED_TYPE("ParameterizedType",false),
	PARAMETERIZED_TYPE_ASSIGNMENT("ParameterizedTypeAssignment",false),
	PARAMETERIZED_VALUE("ParameterizedValue",false),
	PARAMETERIZED_VALUE_ASSIGNMENT("ParameterizedValueAssignment",false),
	PARAMETERIZED_VALUE_SET_TYPE("ParameterizedValueSetType",false),
	PARAMETERIZED_VALUE_SET_TYPE_ASSIGNMENT("ParameterizedValueSetTypeAssignment",false),
	PARTIAL_SPECIFICATION("PartialSpecification",false),
	PATTERN_CONSTRAINT("PatternConstraint",false),
	PERMITTED_ALPHABET("PermittedAlphabet",false),
	PLANE("Plane",false),
	PRESENCE_CONSTRAINT("PresenceConstraint",false),
	PRIMITIVE_FIELD_NAME("PrimitiveFieldName",false),
	PRINTABLE_STRING_TYPE("PrintableString",true),
	QUADRUPLE("Quadruple",false),
	REAL_TYPE("RealType",true),
	REAL_VALUE("RealValue",false),
	REFERENCE("Reference",false),
	REFERENCED_OBJECTS("ReferencedObjects",false),
	REFERENCED_TYPE("ReferencedType",false),
	REFERENCED_VALUE("ReferencedValue",false),
	RELATIVE_OID_COMPONENTS("RelativeOIDComponents",false),
	RELATIVE_OID_COMPONENTS_LIST("RelativeOIDComponentsList",false),
	RELATIVE_OID_TYPE("RelativeOIDType",true),
	RELATIVE_OID_VALUE("RelativeOIDValue",false),
	REQUIRED_TOKEN("RequiredToken",false),
	RESTRICTED_CHARACTER_STRING_TYPE("RestrictedCharacterStringType",false),
	RESTRICTED_CHARACTER_STRING_VALUE("RestrictedCharacterStringValue",false),
	ROOT("Root",false),
	ROOT_ALTERNATIVE_TYPE_LIST("RootAlternativeTypeList",false),
	ROOT_COMPONENT_TYPE_LIST("RootComponentTypeList",false),
	ROOT_ELEMENT_SET_SPEC("RootElementSetSpec",false),
	ROOT_ENUMERATION("RootEnumeration",false),
	ROW("Row",false),
	SELECTION_TYPE("SelectionType",false),
	SEQUENCE_OF_TYPE("SequenceOfType",true),
	SEQUENCE_OF_VALUE("SequenceOfValue",false),
	SEQUENCE_TYPE("SequenceType",true),
	SEQUENCE_VALUE("SequenceValue",false),
	SET_OF_TYPE("SetOfType",true),
	SET_OF_VALUE("SetOfValue",false),
	SET_TYPE("SetType",true),
	SET_VALUE("SetValue",false),
	SETTING("Setting",false),
	SIGNED_NUMBER("SignedNumber",false),
	SIMPLE_DEFINED_TYPE("SimpleDefinedType",false),
	SIMPLE_DEFINED_VALUE("SimpleDefinedValue",false),
	SIMPLE_TABLE_CONSTRAINT("SimpleTableConstraint",false),
	SINGLE_TYPE_CONSTRAINT("SingleTypeConstraint",false),
	SINGLE_VALUE("SingleValue",false),
	SIZE_CONSTRAINT("SizeConstraint",false),
	SPECIAL_REAL_VALUE("SpecialRealValue",false),
	SUBTYPE_CONSTRAINT("SubtypeConstraint",false),
	SUBTYPE_ELEMENTS("SubtypeElements",false),
	SYMBOL("Symbol",false),
	SYMBOL_LIST("SymbolList",false),
	SYMBOLS_EXPORTED("SymbolsExported",false),
	SYMBOLS_FROM_MODULE("SymbolsFromModule",false),
	SYMBOLS_FROM_MODULE_LIST("SymbolsFromModuleList",false),
	SYMBOLS_IMPORTED("SymbolsImported",false),
	SYNTAX_LIST("SyntaxList",false),
	T61_STRING_TYPE("T61String",true),
	TABLE_COLUMN("TableColumn",false),
	TABLE_CONSTRAINT("TableConstraint",false),
	TABLE_ROW("TableRow",false),
	TAG("Tag",false),
	TAG_CLASS("TagClass",false),
	TAG_DEFAULT("TagDefault",false),
	TAGGED_TYPE("TaggedType",false),
	TAGGED_VALUE("TaggedValue",false),
	TELETEX_STRING_TYPE("TeletexString",true),
	TOKEN_OR_GROUP_SPEC("TokenOrGroupSpec",false),
	TUPLE("Tuple",false),
	TYPE("Type",false),
	TYPE_ASSIGNMENT("TypeAssignment",false),
	TYPE_CONSTRAINT("TypeConstraint",false),
	TYPE_CONSTRAINTS("TypeConstraints",false),
	TYPE_FIELD_REFERENCE("typefieldreference",false),
	TYPE_FIELD_SPEC("TypeFieldSpec",false),
	TYPE_FROM_OBJECT("TypeFromObject",false),
	TYPE_OPTIONALITY_SPEC("TypeOptionalitySpec",false),
	TYPE_REFERENCE("typereference",false),
	TYPE_WITH_CONSTRAINT("TypeWithConstraint",false),
	U_ELEMS("UElems",false),
	UNION_MARK("UnionMark",false),
	UNIONS("Unions",false),
	UNIVERSAL_STRING_TYPE("UniversalString",true),
	UNRESTRICTED_CHARACTER_STRING_TYPE("UnrestrictedCharacterStringType",false),
	UNRESTRICTED_CHARACTER_STRING_VALUE("UnrestrictedCharacterStringValue",false),
	UPPER_END_VALUE("UpperEndValue",false),
	UPPER_ENDPOINT("UpperEndpoint",false),
	USEFUL_OBJECT_CLASS_REFERENCE("UsefulObjectClassReference",false),
	USEFUL_TYPE("UsefulType",false),
	USER_DEFINED_CONSTRAINT("UserDefinedConstraint",false),
	USER_DEFINED_CONSTRAINT_PARAMETER("UserDefinedConstraintParameter",false),
	UTC_TIME_TYPE("GeneralizedTime",true),
	UTF8_STRING_TYPE("UTF8String",true),
	VALUE("Value",false),
	VALUE_ASSIGNMENT("ValueAssignment",false),
	VALUE_CONSTRAINT("ValueConstraint",false),
	VALUE_FIELD_REFERENCE("valuefieldreference",false),
	VALUE_FROM_OBJECT("ValueFromObject",false),
	VALUE_LIST("ValueList",false),
	VALUE_OPTIONALITY_SPEC("ValueOptionalitySpec",false),
	VALUE_RANGE("ValueRange",false),
	VALUE_REFERENCE("valuereference",false),
	VALUE_SET("ValueSet",false),
	VALUE_SET_FROM_OBJECTS("ValueSetFromObjects",false),
	VALUE_SET_OPTIONALITY_SPEC("ValueSetOptionalitySpec",false),
	VALUE_SET_TYPE_ASSIGNMENT("ValueSetTypeAssignment",false),
	VARIABLE_TYPE_VALUE_FIELD_SPEC("VariableTypeValueFieldSpec",false),
	VARIABLE_TYPE_VALUE_SET_FIELD_SPEC("VariableTypeValueSetFieldSpec",false),
	VERSION_NUMBER("VersionNumber",false),
	VIDEOTEX_STRING_TYPE("VideotexString",true),
	VISIBLE_STRING_TYPE("VisibleString",true),
	WITH_SYNTAX_SPEC("WithSyntaxSpec",false)
	;
	
	/** true if this production represents a built-in type */
	private final boolean isBuiltInType;
	/** name of the current production in the grammar */
	private final String productionName;
	
	/**
	 * Create an Ast Production
	 * @param productionName - name of the production
	 * @param type - true if the production is a built-in type
	 */
	private AsnProduction(String productionName,boolean type) {
		this.productionName = productionName;
		this.isBuiltInType = type;
		return;
	}
	
	/**
	 * Get the name of the production
	 * @return - name of the production
	 */
	public Object astValue() {
		return productionName;
	}
	
	/**
	 * Get an AST node corresponding to this production
	 * @return - AST node corresponding to this production
	 */
	public AsnNode getNode() {
		switch(this) {
		case MODULE_DEFINITION:
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Module Definition requires a name");
		default:
			return new AsnNode(this);
		}
	}
	
	/**
	 * Get an AST node corresponding to this production
	 * @param nameToken - token containing name of this production
	 * @return - AST node corresponding to this production
	 */
	public AsnNode getNode(Token nameToken) {
		switch(this) {
		case MODULE_DEFINITION:
			return new AsnModule(nameToken);
		default:
			return new AsnNode(this,nameToken);
		}
	}
	
	/**
	 * Get an AST node corresponding to this production
	 * @param name - name token associated with the production
	 * @param value - value token associated with the production
	 * @return AST node corresponding to this production
	 */
	public AsnNode getNode(Token name, Token value) {
		switch(this) {
		case MODULE_DEFINITION:
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Module Definition does not accept value");
		default:
			return new AsnNode(this,name,value);
		}
	}
	
	/**
	 * Get an AST node corresponding to this production
	 * @param name - name of the production
	 * @param isField - true if the production is an ObjectClass field, false otherwise 
	 * @return AST node corresponding to this production
	 */
	public AsnNode getNode(Token name, boolean isField) {
		switch(this) {
		case MODULE_DEFINITION:
			throw new ModelException(ExceptionReason.INVALID_PARAMETER,"Module Definition does not accept field value");
		default:
			return new AsnNode(this,name,isField);
		}
	}
	
	/**
	 * check if this production represents a built-in type
	 * @return - true if the production is a built-in type, false otherwise
	 */
	public boolean isBuiltIn() {
		return isBuiltInType;
	}
	
	/**
	 * Lookup the production corresponding to a name
	 * @param s - name of the production
	 * @return - AstProduction. Null if no such production
	 */
	public static AsnProduction lookup(String s) {
		for(AsnProduction p : AsnProduction.values()) {
			if(p.productionName.equals(s)) return p;
		}
		return AsnProduction.valueOf(s);
	}
}
