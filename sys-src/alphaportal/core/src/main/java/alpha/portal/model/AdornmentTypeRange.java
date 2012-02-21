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

import org.apache.commons.lang.ArrayUtils;

/**
 * The Class AdornmentTypeRange.
 */
public class AdornmentTypeRange {

	/** The max integer. */
	private int maxInteger;

	/** The min integer. */
	private int minInteger;

	/** The max float. */
	private float maxFloat;

	/** The min float. */
	private float minFloat;

	/** The valid strings. */
	private String[] validStrings;

	/**
	 * Instantiates a new adornment type range.
	 * 
	 * @param min
	 *            the min
	 * @param max
	 *            the max
	 */
	public AdornmentTypeRange(final int min, final int max) {
		this.maxInteger = max;
		this.minInteger = min;
	}

	/**
	 * Instantiates a new adornment type range.
	 * 
	 * @param min
	 *            the min
	 * @param max
	 *            the max
	 */
	public AdornmentTypeRange(final float min, final float max) {
		this.maxFloat = max;
		this.minFloat = min;
	}

	/**
	 * Instantiates a new adornment type range.
	 * 
	 * @param validStrings
	 *            the valid strings
	 */
	public AdornmentTypeRange(final String[] validStrings) {
		this.setValidStrings(validStrings);
	}

	/**
	 * Checks if is valid.
	 * 
	 * @param value
	 *            the value
	 * @return true, if is valid
	 */
	public boolean isValid(final int value) {
		if ((value <= this.maxInteger) && (value >= this.minInteger))
			return true;

		return false;
	}

	/**
	 * Checks if is valid.
	 * 
	 * @param value
	 *            the value
	 * @return true, if is valid
	 */
	public boolean isValid(final float value) {
		if ((value <= this.maxFloat) && (value >= this.minFloat))
			return true;

		return false;
	}

	/**
	 * Checks if is valid.
	 * 
	 * @param value
	 *            the value
	 * @return true, if is valid
	 */
	public boolean isValid(final String value) {
		return ArrayUtils.contains(this.validStrings, value);
	}

	/**
	 * Gets the max integer.
	 * 
	 * @return the max integer
	 */
	public int getMaxInteger() {
		return this.maxInteger;
	}

	/**
	 * Sets the max integer.
	 * 
	 * @param maxInteger
	 *            the new max integer
	 */
	public void setMaxInteger(final int maxInteger) {
		this.maxInteger = maxInteger;
	}

	/**
	 * Gets the min integer.
	 * 
	 * @return the min integer
	 */
	public int getMinInteger() {
		return this.minInteger;
	}

	/**
	 * Sets the min integer.
	 * 
	 * @param minInteger
	 *            the new min integer
	 */
	public void setMinInteger(final int minInteger) {
		this.minInteger = minInteger;
	}

	/**
	 * Gets the max float.
	 * 
	 * @return the max float
	 */
	public float getMaxFloat() {
		return this.maxFloat;
	}

	/**
	 * Sets the max float.
	 * 
	 * @param maxFloat
	 *            the new max float
	 */
	public void setMaxFloat(final float maxFloat) {
		this.maxFloat = maxFloat;
	}

	/**
	 * Gets the min float.
	 * 
	 * @return the min float
	 */
	public float getMinFloat() {
		return this.minFloat;
	}

	/**
	 * Sets the min float.
	 * 
	 * @param minFloat
	 *            the new min float
	 */
	public void setMinFloat(final float minFloat) {
		this.minFloat = minFloat;
	}

	/**
	 * Gets the valid strings.
	 * 
	 * @return the valid strings
	 */
	public String[] getValidStrings() {
		return (String[]) ArrayUtils.clone(this.validStrings);
	}

	/**
	 * Sets the valid strings.
	 * 
	 * @param validStrings
	 *            the new valid strings
	 */
	public void setValidStrings(final String[] validStrings) {
		this.validStrings = (String[]) ArrayUtils.clone(validStrings);
	}

}
