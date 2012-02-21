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
 * The Enum AdornmentTypeDataProvision.
 */
public enum AdornmentTypeDataProvision {

	/** The OPEN. */
	OPEN("Offen"),
	/** The INPROGRESS. */
	INPROGRESS("In Arbeit"),
	/** The FULLFILLED. */
	FULLFILLED("Erfüllt");

	/** The name. */
	private final String name;

	/** The data value type. */
	private AdornmentValueType dataValueType;

	/**
	 * Instantiates a new AdornmentType.
	 * 
	 * @param name
	 *            the value of the specific adornment.
	 */
	AdornmentTypeDataProvision(final String name) {
		this.name = name;
	}

	/**
	 * gets the value of the enum list.
	 * 
	 * @return the value
	 */
	public String value() {
		return this.name;
	}

	/**
	 * Getter for name (needed by jsp).
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
	 * @param value
	 *            the value of the adornment.
	 * @return an AdornmentType object.
	 */
	public static AdornmentTypeDataProvision fromValue(final String value) {
		for (final AdornmentTypeDataProvision adornmentTypeVisibility : AdornmentTypeDataProvision
				.values()) {
			if (adornmentTypeVisibility.name.equals(value))
				return adornmentTypeVisibility;
		}
		return null;
	}
}