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
package alpha.portal.webapp.taglib;

import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.jsp.tagext.TagData;
import javax.servlet.jsp.tagext.TagExtraInfo;
import javax.servlet.jsp.tagext.VariableInfo;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.appfuse.Constants;

/**
 * Implementation of <code>TagExtraInfo</code> for the <b>constants</b> tag,
 * identifying the scripting object(s) to be made visible.
 * 
 * @author Matt Raible
 */
public class ConstantsTei extends TagExtraInfo {

	/** The log. */
	private final Log log = LogFactory.getLog(ConstantsTei.class);

	/**
	 * Return information about the scripting variables to be created.
	 * 
	 * @param data
	 *            the input data
	 * @return VariableInfo array of variable information
	 */
	@Override
	public VariableInfo[] getVariableInfo(final TagData data) {
		// loop through and expose all attributes
		final List<VariableInfo> vars = new ArrayList<VariableInfo>();

		try {
			String clazz = data.getAttributeString("className");

			if (clazz == null) {
				clazz = Constants.class.getName();
			}

			final Class c = Class.forName(clazz);

			// if no var specified, get all
			if (data.getAttributeString("var") == null) {
				final Field[] fields = c.getDeclaredFields();

				AccessibleObject.setAccessible(fields, true);

				for (final Field field : fields) {
					final String type = field.getType().getName();
					vars.add(new VariableInfo(field.getName(), ((field
							.getType().isArray()) ? type.substring(2,
							type.length() - 1)
							+ "[]" : type), true, VariableInfo.AT_END));
				}
			} else {
				final String var = data.getAttributeString("var");
				final String type = c.getField(var).getType().getName();
				vars.add(new VariableInfo(c.getField(var).getName(), ((c
						.getField(var).getType().isArray()) ? type.substring(2,
						type.length() - 1) + "[]" : type), true,
						VariableInfo.AT_END));
			}
		} catch (final Exception cnf) {
			this.log.error(cnf.getMessage());
			cnf.printStackTrace();
		}

		return vars.toArray(new VariableInfo[] {});
	}
}
