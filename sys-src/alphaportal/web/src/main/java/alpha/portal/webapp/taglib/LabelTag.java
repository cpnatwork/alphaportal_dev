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

import java.io.IOException;
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.Tag;
import javax.servlet.jsp.tagext.TagSupport;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.validator.Field;
import org.apache.commons.validator.Form;
import org.apache.commons.validator.ValidatorResources;
import org.springframework.beans.factory.BeanFactoryUtils;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.MessageSource;
import org.springframework.context.NoSuchMessageException;
import org.springframework.validation.Errors;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.WebApplicationContextUtils;
import org.springframework.web.servlet.DispatcherServlet;
import org.springframework.web.servlet.support.RequestContext;
import org.springmodules.validation.commons.ValidatorFactory;

/**
 * <p>
 * This class is designed to render a <label> tag for labeling your forms and
 * adds an asterik (*) for required fields. It was originally written by Erik
 * Hatcher (http://www.ehatchersolutions.com/JavaDevWithAnt/).
 * 
 * <p>
 * It is designed to be used as follows:
 * 
 * <pre>
 * &lt;tag:label key="userForm.username"/&gt;
 * </pre>
 * 
 * @jsp.tag name="label" bodycontent="empty"
 */
public class LabelTag extends TagSupport {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5310144023136517119L;

	/** The request context. */
	protected RequestContext requestContext;

	/** The log. */
	protected transient final Log log = LogFactory.getLog(LabelTag.class);

	/** The key. */
	protected String key = null;

	/** The style class. */
	protected String styleClass = null;

	/** The error class. */
	protected String errorClass = null;

	/** The colon. */
	protected boolean colon = false;

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.servlet.jsp.tagext.TagSupport#doStartTag()
	 */
	@Override
	public int doStartTag() throws JspException {

		try {
			this.requestContext = new RequestContext(
					(HttpServletRequest) this.pageContext.getRequest());
		} catch (final RuntimeException ex) {
			throw ex;
		} catch (final Exception ex) {
			this.pageContext.getServletContext().log("Exception in custom tag",
					ex);
		}

		// Look up this key to see if its a field of the current form
		boolean requiredField = false;
		boolean validationError = false;

		final ValidatorResources resources = this.getValidatorResources();

		Locale locale = this.pageContext.getRequest().getLocale();

		if (locale == null) {
			locale = Locale.getDefault();
		}

		// get the name of the bean from the key
		final String formName = this.key.substring(0, this.key.indexOf('.'));
		final String fieldName = this.key.substring(formName.length() + 1);

		if (resources != null) {
			final Form form = resources.getForm(locale, formName);

			if (form != null) {
				final Field field = form.getField(fieldName);

				if (field != null) {
					if (field.isDependency("required")
							|| field.isDependency("validwhen")) {
						requiredField = true;
					}
				}
			}
		}

		final Errors errors = this.requestContext.getErrors(formName, false);
		List fes = null;
		if (errors != null) {
			fes = errors.getFieldErrors(fieldName);
			// String errorMsg = getErrorMessages(fes);
		}

		if ((fes != null) && (fes.size() > 0)) {
			validationError = true;
		}

		// Retrieve the message string we are looking for
		String message = null;
		try {
			message = this.getMessageSource()
					.getMessage(this.key, null, locale);
		} catch (final NoSuchMessageException nsm) {
			message = "???" + this.key + "???";
		}

		String cssClass = null;
		if (this.styleClass != null) {
			cssClass = this.styleClass;
		} else if (requiredField) {
			cssClass = "required";
		}

		final String cssErrorClass = (this.errorClass != null) ? this.errorClass
				: "error";
		final StringBuffer label = new StringBuffer();

		if ((message == null) || "".equals(message.trim())) {
			label.append("");
		} else {
			label.append("<label for=\"").append(fieldName).append("\"");

			if (validationError) {
				label.append(" class=\"").append(cssErrorClass).append("\"");
			} else if (cssClass != null) {
				label.append(" class=\"").append(cssClass).append("\"");
			}

			label.append(">").append(message);
			label.append((requiredField) ? " <span class=\"req\">*</span>" : "");
			label.append((this.colon) ? ":" : "");
			label.append("</label>");

			if (validationError) {
				label.append("<img class=\"validationWarning\" alt=\"");
				label.append(this.getMessageSource().getMessage("icon.warning",
						null, locale));
				label.append("\"");

				final String context = ((HttpServletRequest) this.pageContext
						.getRequest()).getContextPath();

				label.append(" src=\"").append(context);
				label.append(this.getMessageSource().getMessage(
						"icon.warning.img", null, locale));
				label.append("\" />");
			}
		}

		// Print the retrieved message to our output writer
		try {
			this.writeMessage(label.toString());
		} catch (final IOException io) {
			io.printStackTrace();
			throw new JspException("Error writing label: " + io.getMessage());
		}

		// Continue processing this page
		return (Tag.SKIP_BODY);
	}

	/**
	 * Extract the error messages from the given ObjectError list.
	 * 
	 * @param msg
	 *            the msg
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	/*
	 * private String getErrorMessages(List fes) throws NoSuchMessageException {
	 * StringBuffer message = new StringBuffer(); for (int i = 0; i <
	 * fes.size(); i++) { ObjectError error = (ObjectError) fes.get(i);
	 * message.append(this.requestContext.getMessage(error, true)); } return
	 * message.toString(); }
	 */

	/**
	 * Write the message to the page.
	 * 
	 * @param msg
	 *            the message to write
	 * @throws IOException
	 *             if writing failed
	 */
	protected void writeMessage(final String msg) throws IOException {
		this.pageContext.getOut().write(msg);
	}

	/**
	 * Sets the key.
	 * 
	 * @param key
	 *            the new key
	 * @jsp.attribute required="true" rtexprvalue="true"
	 */
	public void setKey(final String key) {
		this.key = key;
	}

	/**
	 * Setter for specifying whether to include colon.
	 * 
	 * @param colon
	 *            the new colon
	 * @jsp.attribute required="false" rtexprvalue="true"
	 */
	public void setColon(final boolean colon) {
		this.colon = colon;
	}

	/**
	 * Setter for assigning a CSS class, default is label.
	 * 
	 * @param styleClass
	 *            the new style class
	 * @jsp.attribute required="false" rtexprvalue="true"
	 */
	public void setStyleClass(final String styleClass) {
		this.styleClass = styleClass;
	}

	/**
	 * Setter for assigning a CSS class when errors occur, defaults to
	 * labelError.
	 * 
	 * @param errorClass
	 *            the new error class
	 * @jsp.attribute required="false" rtexprvalue="true"
	 */
	public void setErrorClass(final String errorClass) {
		this.errorClass = errorClass;
	}

	/**
	 * Release all allocated resources.
	 */
	@Override
	public void release() {
		super.release();
		this.key = null;
		this.colon = false;
		this.styleClass = null;
		this.errorClass = null;
	}

	/**
	 * Get the validator resources from a ValidatorFactory defined in the web
	 * application context or one of its parent contexts. The bean is resolved
	 * by type (org.springframework.validation.commons.ValidatorFactory).
	 * 
	 * @return ValidatorResources from a ValidatorFactory.
	 */
	private ValidatorResources getValidatorResources() {
		// look in servlet beans definition (i.e. action-servlet.xml)
		WebApplicationContext ctx = (WebApplicationContext) this.pageContext
				.getRequest().getAttribute(
						DispatcherServlet.WEB_APPLICATION_CONTEXT_ATTRIBUTE);
		ValidatorFactory factory = null;
		try {
			factory = BeanFactoryUtils.beanOfTypeIncludingAncestors(ctx,
					ValidatorFactory.class, true, true);
		} catch (final NoSuchBeanDefinitionException e) {
			// look in main application context (i.e. applicationContext.xml)
			ctx = WebApplicationContextUtils
					.getRequiredWebApplicationContext(this.pageContext
							.getServletContext());
			factory = BeanFactoryUtils.beanOfTypeIncludingAncestors(ctx,
					ValidatorFactory.class, true, true);
		}
		return factory.getValidatorResources();
	}

	/**
	 * Use the application context itself for default message resolution.
	 * 
	 * @return the message source
	 */
	private MessageSource getMessageSource() {
		return this.requestContext.getWebApplicationContext();
	}
}
