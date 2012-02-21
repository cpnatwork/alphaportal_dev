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
import java.util.HashMap;
import java.util.Map;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.PageContext;
import javax.servlet.jsp.tagext.Tag;
import javax.servlet.jsp.tagext.TagSupport;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.appfuse.Constants;

/**
 * <p>
 * This class is designed to put all the public variables in a class to a
 * specified scope - designed for exposing a Constants class to Tag Libraries.
 * </p>
 * 
 * <p>
 * It is designed to be used as follows:
 * 
 * <pre>
 * &lt;tag:constants /&gt;
 * </pre>
 * 
 * </p>
 * 
 * <p>
 * Optional values are "className" (fully qualified) and "scope".
 * </p>
 * 
 * <p>
 * <a href="BaseAction.java.html"><i>View Source</i></a>
 * </p>
 * 
 * @author <a href="mailto:matt@raibledesigns.com">Matt Raible</a>
 */
public class ConstantsTag extends TagSupport {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3258417209566116146L;

	/** The log. */
	private final Log log = LogFactory.getLog(ConstantsTag.class);

	/**
	 * The class to expose the variables from.
	 */
	private String clazz = Constants.class.getName();

	/**
	 * The scope to be put the variable in.
	 */
	protected String scope;

	/**
	 * The single variable to expose.
	 */
	protected String var;

	/**
	 * Main method that does processing and exposes Constants in specified
	 * scope.
	 * 
	 * @return int
	 * @throws JspException
	 *             if processing fails
	 */
	@Override
	public int doStartTag() throws JspException {
		// Using reflection, get the available field names in the class
		Class c = null;
		int toScope = PageContext.PAGE_SCOPE;

		if (this.scope != null) {
			toScope = this.getScope(this.scope);
		}

		try {
			c = Class.forName(this.clazz);
		} catch (final ClassNotFoundException cnf) {
			this.log.error("ClassNotFound - maybe a typo?");
			throw new JspException(cnf.getMessage());
		}

		try {
			// if var is null, expose all variables
			if (this.var == null) {
				final Field[] fields = c.getDeclaredFields();

				AccessibleObject.setAccessible(fields, true);

				for (final Field field : fields) {
					this.pageContext.setAttribute(field.getName(),
							field.get(this), toScope);
				}
			} else {
				try {
					final Object value = c.getField(this.var).get(this);
					this.pageContext.setAttribute(c.getField(this.var)
							.getName(), value, toScope);
				} catch (final NoSuchFieldException nsf) {
					this.log.error(nsf.getMessage());
					throw new JspException(nsf);
				}
			}
		} catch (final IllegalAccessException iae) {
			this.log.error("Illegal Access Exception - maybe a classloader issue?");
			throw new JspException(iae);
		}

		// Continue processing this page
		return (Tag.SKIP_BODY);
	}

	/**
	 * Sets the class name.
	 * 
	 * @param clazz
	 *            the new class name
	 */
	public void setClassName(final String clazz) {
		this.clazz = clazz;
	}

	/**
	 * Gets the class name.
	 * 
	 * @return the class name
	 */
	public String getClassName() {
		return this.clazz;
	}

	/**
	 * Sets the scope to be put the variable in.
	 * 
	 * @param scope
	 *            the new scope to be put the variable in
	 */
	public void setScope(final String scope) {
		this.scope = scope;
	}

	/**
	 * Gets the scope to be put the variable in.
	 * 
	 * @return the scope to be put the variable in
	 */
	public String getScope() {
		return (this.scope);
	}

	/**
	 * Sets the single variable to expose.
	 * 
	 * @param var
	 *            the new single variable to expose
	 */
	public void setVar(final String var) {
		this.var = var;
	}

	/**
	 * Gets the single variable to expose.
	 * 
	 * @return the single variable to expose
	 */
	public String getVar() {
		return (this.var);
	}

	/**
	 * Release all allocated resources.
	 */
	@Override
	public void release() {
		super.release();
		this.clazz = null;
		this.scope = Constants.class.getName();
	}

	/**
	 * Maps lowercase JSP scope names to their PageContext integer constant
	 * values.
	 */
	private static final Map<String, Integer> SCOPES = new HashMap<String, Integer>();

	/**
	 * Initialize the scope names map and the encode variable
	 */
	static {
		ConstantsTag.SCOPES.put("page", PageContext.PAGE_SCOPE);
		ConstantsTag.SCOPES.put("request", PageContext.REQUEST_SCOPE);
		ConstantsTag.SCOPES.put("session", PageContext.SESSION_SCOPE);
		ConstantsTag.SCOPES.put("application", PageContext.APPLICATION_SCOPE);
	}

	/**
	 * Converts the scope name into its corresponding PageContext constant
	 * value.
	 * 
	 * @param scopeName
	 *            Can be "page", "request", "session", or "application" in any
	 *            case.
	 * @return The constant representing the scope (ie.
	 *         PageContext.REQUEST_SCOPE).
	 * @throws JspException
	 *             if the scopeName is not a valid name.
	 */
	public int getScope(final String scopeName) throws JspException {
		final Integer scope = ConstantsTag.SCOPES.get(scopeName.toLowerCase());

		if (scope == null)
			throw new JspException("Scope '" + scopeName
					+ "' not a valid option");

		return scope;
	}
}
