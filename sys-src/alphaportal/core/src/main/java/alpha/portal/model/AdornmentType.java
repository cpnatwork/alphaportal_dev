/**************************************************************************
 * alpha-Portal: A web portal, for managing knowledge-driven 
 * ad-hoc processes, in form of case files.
 * ==============================================
 * Copyright (C) 2011-2012 by 
 *   - Christoph P. Neumann (http://www.chr15t0ph.de)
 *   - and the SWAT 2011 team
 **************************************************************************
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *     http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software 
 * distributed under the License is distributed on an "AS IS" BASIS, 
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 **************************************************************************
 * $Id$
 *************************************************************************/
package alpha.portal.model;

/**
 * Adornment type is a list of default adornments, which will be generated, if a
 * new α-Card is created.
 */

public enum AdornmentType {

	/**
	 * The adornment "Title", which defines the name of the adornment.
	 */
	Title("AlphaCard Titel", "no Titel"),
	/**
	 * The adornment "Contributor", which defines the responsible person of this
	 * α-Card.
	 */
	Contributor("Verantwortlicher", AdornmentValueType.Enum),
	/**
	 * The adornment "ContributorRole", which defines the role of the
	 * responsible person of this α-Card.
	 */
	ContributorRole("Fachgebiet des Verantwortlichen", AdornmentValueType.Enum),

	/**
	 * the adornment "Visibility", which defines the visibility of the payload
	 * of this α-Card.
	 */
	Visibility("Sichtbarkeit", AdornmentValueType.Enum,
			AdornmentTypeVisibility.PRIVATE.value(), new AdornmentTypeRange(
					new String[] { AdornmentTypeVisibility.PRIVATE.value(),
							AdornmentTypeVisibility.PUBLIC.value() })),

	/**
	 * the adornment "Validity", which defines the validity of the payload of
	 * this α-Card.
	 */
	Validity("Gültigkeit", AdornmentValueType.Enum,
			AdornmentTypeValidity.INVALID.value(), new AdornmentTypeRange(
					new String[] { AdornmentTypeValidity.VALID.value(),
							AdornmentTypeValidity.INVALID.value() })),

	/**
	 * the adornment "Deleted", which defines if this α-Card was deleted.
	 */
	Deleted("Gelöscht", AdornmentValueType.Enum, AdornmentTypeDeleted.FALSE
			.value(), new AdornmentTypeRange(new String[] {
			AdornmentTypeDeleted.FALSE.value(),
			AdornmentTypeDeleted.TRUE.value() })),

	/**
	 * the adornment "Data Provision", which defines the provision of the
	 * payload of this α-Card.
	 */
	DataProvision("Zustand", AdornmentValueType.Enum,
			AdornmentTypeDataProvision.OPEN.value(), new AdornmentTypeRange(
					new String[] { AdornmentTypeDataProvision.OPEN.value(),
							AdornmentTypeDataProvision.INPROGRESS.value(),
							AdornmentTypeDataProvision.FULLFILLED.value() })),

	/**
	 * The adornment "Title", which defines the name of the adornment.
	 */
	PayloadVersionNumber("Payload Versionsnummer", AdornmentValueType.Integer,
			"0", new AdornmentTypeRange(0, Integer.MAX_VALUE));

	/**
	 * The value of the specific adornment.
	 */
	private final String name;

	/** the types a value can be (Integer, Float, String or Enum). */
	private AdornmentValueType valueType;

	/** the range a value can be. */
	private AdornmentTypeRange valueRange;

	/**
	 * the defaultValue of the adornment.
	 */
	private String valueDefault;

	/**
	 * Instantiates a new AdornmentType.
	 * 
	 * @param name
	 *            the value of the specific adornment.
	 */
	AdornmentType(final String name) {
		this(name, AdornmentValueType.String);
	}

	/**
	 * Instantiates a new AdornmentType with valueType.
	 * 
	 * @param name
	 *            the value of the specific adornment.
	 * @param type
	 *            the type
	 */
	AdornmentType(final String name, final AdornmentValueType type) {
		this(name, type, "");
	}

	/**
	 * Instantiates a new AdornmentType.
	 * 
	 * @param name
	 *            the value of the specific adornment.
	 * @param defaultValue
	 *            the default value
	 */
	AdornmentType(final String name, final String defaultValue) {
		this(name, AdornmentValueType.String, defaultValue);
	}

	/**
	 * Instantiates a new AdornmentType.
	 * 
	 * @param name
	 *            the value of the specific adornment.
	 * @param type
	 *            the type
	 * @param defaultValue
	 *            the default value
	 */
	AdornmentType(final String name, final AdornmentValueType type,
			final String defaultValue) {
		this(name, type, defaultValue, null);
	}

	/**
	 * Instantiates a new AdornmentType.
	 * 
	 * @param name
	 *            the value of the specific adornment.
	 * @param type
	 *            the type
	 * @param defaultValue
	 *            the default value of the adornment.
	 * @param range
	 *            the valid range of the adornment values.
	 */
	AdornmentType(final String name, final AdornmentValueType type,
			final String defaultValue, final AdornmentTypeRange range) {
		this.name = name;
		this.valueType = type;
		this.valueDefault = defaultValue;
		this.valueRange = range;
	}

	/**
	 * gets the name of the enum list.
	 * 
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * goes through the list of default adornments and returns an object of the
	 * type AdornmentType, if the given value equals with one out of the lists.
	 * 
	 * @param name
	 *            the value of the adornment.
	 * @return an AdornmentType object.
	 */
	public static AdornmentType fromName(final String name) {
		for (final AdornmentType adornmentType : AdornmentType.values()) {
			if (adornmentType.name.equals(name))
				return adornmentType;
		}
		return null;
	}

	/**
	 * Gets the types a value can be (Integer, Float, String or Enum).
	 * 
	 * @return the types a value can be (Integer, Float, String or Enum)
	 */
	public AdornmentValueType getValueType() {
		return this.valueType;
	}

	/**
	 * Sets the types a value can be (Integer, Float, String or Enum).
	 * 
	 * @param valueType
	 *            the new types a value can be (Integer, Float, String or Enum)
	 */
	public void setValueType(final AdornmentValueType valueType) {
		this.valueType = valueType;
	}

	/**
	 * Gets the range a value can be.
	 * 
	 * @return the range a value can be
	 */
	public AdornmentTypeRange getValueRange() {
		return this.valueRange;
	}

	/**
	 * Sets the range a value can be.
	 * 
	 * @param valueRange
	 *            the new range a value can be
	 */
	public void setValueRange(final AdornmentTypeRange valueRange) {
		this.valueRange = valueRange;
	}

	/**
	 * Gets the defaultValue of the adornment.
	 * 
	 * @return the defaultValue of the adornment
	 */
	public String getValueDefault() {
		return this.valueDefault;
	}

	/**
	 * Sets the defaultValue of the adornment.
	 * 
	 * @param valueDefault
	 *            the new defaultValue of the adornment
	 */
	public void setValueDefault(final String valueDefault) {
		this.valueDefault = valueDefault;
	}

	/**
	 * Validate.
	 * 
	 * @param value
	 *            the value
	 * @return true, if successful
	 */
	public boolean validate(final String value) {
		if (this.valueRange == null)
			return true;
		switch (this.valueType) {
		case Enum:
			return this.valueRange.isValid(value);
		case Float:
			try {
				return this.valueRange.isValid(Float.parseFloat(value));
			} catch (final NumberFormatException e) {
				return false;
			}
		case Integer:
			try {
				return this.valueRange.isValid(Integer.parseInt(value));
			} catch (final NumberFormatException e) {
				return false;
			}
		case String:
			return true;
		}
		return false;
	}
}
