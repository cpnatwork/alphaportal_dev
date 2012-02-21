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

import org.apache.commons.validator.Field;
import org.apache.commons.validator.GenericValidator;
import org.apache.commons.validator.ValidatorAction;
import org.apache.commons.validator.util.ValidatorUtils;
import org.springframework.validation.Errors;
import org.springmodules.validation.commons.FieldChecks;

/**
 * ValidationUtil Helper class for performing custom validations that aren't
 * already included in the core Commons Validator.
 * 
 * <p>
 * <a href="ValidationUtil.java.html"><i>View Source</i></a>
 * </p>
 * 
 * @author <a href="mailto:matt@raibledesigns.com">Matt Raible</a>
 */
public class ValidationUtil {
	// ~ Methods
	// ================================================================

	/**
	 * Validates that two fields match.
	 * 
	 * @param bean
	 *            the bean
	 * @param va
	 *            the va
	 * @param field
	 *            the field
	 * @param errors
	 *            the errors
	 * @return true, if successful
	 */
	public static boolean validateTwoFields(final Object bean,
			final ValidatorAction va, final Field field, final Errors errors) {
		final String value = ValidatorUtils.getValueAsString(bean,
				field.getProperty());
		final String sProperty2 = field.getVarValue("secondProperty");
		final String value2 = ValidatorUtils.getValueAsString(bean, sProperty2);

		if (!GenericValidator.isBlankOrNull(value)) {
			try {
				if (!value.equals(value2)) {
					FieldChecks.rejectValue(errors, field, va);
					return false;
				}
			} catch (final Exception e) {
				FieldChecks.rejectValue(errors, field, va);
				return false;
			}
		}

		return true;
	}
}
