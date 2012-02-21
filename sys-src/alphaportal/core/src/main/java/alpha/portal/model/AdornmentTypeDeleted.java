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
 * The Enum AdornmentTypeDeleted.
 */
public enum AdornmentTypeDeleted {

	/** The TRUE. */
	TRUE("Ja"),
	/** The FALSE. */
	FALSE("Nein");

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
	AdornmentTypeDeleted(final String name) {
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
	 * goes through the list of default adornments and returns an object of the
	 * type AdornmentType, if the given value equals with one out of the lists.
	 * 
	 * @param value
	 *            the value of the adornment.
	 * @return an AdornmentType object.
	 */
	public static AdornmentTypeDeleted fromValue(final String value) {
		for (final AdornmentTypeDeleted adornmentTypeDeleted : AdornmentTypeDeleted
				.values()) {
			if (adornmentTypeDeleted.name.equals(value))
				return adornmentTypeDeleted;
		}
		return null;
	}
}