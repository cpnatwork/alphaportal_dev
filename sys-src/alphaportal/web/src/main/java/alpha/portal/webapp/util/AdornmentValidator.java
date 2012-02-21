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
package alpha.portal.webapp.util;

import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;

/**
 * The Class AdornmentValidator.
 */
public class AdornmentValidator implements Validator {

	/** The type. */
	private final AdornmentType type;

	/**
	 * Instantiates a new adornment validator.
	 * 
	 * @param t
	 *            the t
	 */
	public AdornmentValidator(final AdornmentType t) {
		this.type = t;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.springframework.validation.Validator#supports(java.lang.Class)
	 */
	public boolean supports(final Class<?> clazz) {
		return Adornment.class.equals(clazz);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.springframework.validation.Validator#validate(java.lang.Object,
	 * org.springframework.validation.Errors)
	 */
	public void validate(final Object target, final Errors errors) {
		final Adornment a = (Adornment) target;
		if (!this.type.validate(a.getValue())) {
			errors.rejectValue("value", "adornment.invalidValue");
		}
	}

}
