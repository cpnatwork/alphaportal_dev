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
import java.text.Collator;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.TagSupport;

import org.appfuse.model.LabelValue;
import org.displaytag.tags.el.ExpressionEvaluator;

/**
 * Tag for creating multiple &lt;select&gt; options for displaying a list of
 * country names.
 * 
 * <p>
 * <b>NOTE</b> - This tag requires a Java2 (JDK 1.2 or later) platform.
 * </p>
 * 
 * @author Jens Fischer, Matt Raible
 * @version $Revision$ $Date$
 * 
 * @jsp.tag name="country" bodycontent="empty"
 */
public class CountryTag extends TagSupport {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3905528206810167095L;

	/** The name. */
	private String name;

	/** The prompt. */
	private String prompt;

	/** The scope. */
	private String scope;

	/** The selected. */
	private String selected;

	/**
	 * Sets the name.
	 * 
	 * @param name
	 *            The name to set.
	 * @jsp.attribute required="false" rtexprvalue="true"
	 */
	public void setName(final String name) {
		this.name = name;
	}

	/**
	 * Sets the prompt.
	 * 
	 * @param prompt
	 *            The prompt to set.
	 * @jsp.attribute required="false" rtexprvalue="true"
	 */
	public void setPrompt(final String prompt) {
		this.prompt = prompt;
	}

	/**
	 * Sets the default.
	 * 
	 * @param selected
	 *            The selected option.
	 * @jsp.attribute required="false" rtexprvalue="true"
	 */
	public void setDefault(final String selected) {
		this.selected = selected;
	}

	/**
	 * Property used to simply stuff the list of countries into a specified
	 * scope.
	 * 
	 * @param scope
	 *            the new to scope
	 * @jsp.attribute required="false" rtexprvalue="true"
	 */
	public void setToScope(final String scope) {
		this.scope = scope;
	}

	/**
	 * Process the start of this tag.
	 * 
	 * @return int status
	 * @throws JspException
	 *             if a JSP exception has occurred
	 * @see javax.servlet.jsp.tagext.Tag#doStartTag()
	 */
	@Override
	public int doStartTag() throws JspException {
		final ExpressionEvaluator eval = new ExpressionEvaluator(this,
				this.pageContext);

		if (this.selected != null) {
			this.selected = eval.evalString("default", this.selected);
		}

		final Locale userLocale = this.pageContext.getRequest().getLocale();
		final List countries = this.buildCountryList(userLocale);

		if (this.scope != null) {
			if (this.scope.equals("page")) {
				this.pageContext.setAttribute(this.name, countries);
			} else if (this.scope.equals("request")) {
				this.pageContext.getRequest()
						.setAttribute(this.name, countries);
			} else if (this.scope.equals("session")) {
				this.pageContext.getSession()
						.setAttribute(this.name, countries);
			} else if (this.scope.equals("application")) {
				this.pageContext.getServletContext().setAttribute(this.name,
						countries);
			} else
				throw new JspException(
						"Attribute 'scope' must be: page, request, session or application");
		} else {
			final StringBuffer sb = new StringBuffer();
			sb.append("<select name=\"" + this.name + "\" id=\"" + this.name
					+ "\" class=\"select\">\n");

			if (this.prompt != null) {
				sb.append("    <option value=\"\" selected=\"selected\">");
				sb.append(eval.evalString("prompt", this.prompt)
						+ "</option>\n");
			}

			for (final Iterator i = countries.iterator(); i.hasNext();) {
				final LabelValue country = (LabelValue) i.next();
				sb.append("    <option value=\"" + country.getValue() + "\"");

				if ((this.selected != null)
						&& this.selected.equals(country.getValue())) {
					sb.append(" selected=\"selected\"");
				}

				sb.append(">" + country.getLabel() + "</option>\n");
			}

			sb.append("</select>");

			try {
				this.pageContext.getOut().write(sb.toString());
			} catch (final IOException io) {
				throw new JspException(io);
			}
		}

		return super.doStartTag();
	}

	/**
	 * Release aquired resources to enable tag reusage.
	 * 
	 * @see javax.servlet.jsp.tagext.Tag#release()
	 */
	@Override
	public void release() {
		super.release();
	}

	/**
	 * Build a List of LabelValues for all the available countries. Uses the two
	 * letter uppercase ISO name of the country as the value and the localized
	 * country name as the label.
	 * 
	 * @param locale
	 *            The Locale used to localize the country names.
	 * 
	 * @return List of LabelValues for all available countries.
	 */
	protected List buildCountryList(final Locale locale) {
		final String EMPTY = "";
		final Locale[] available = Locale.getAvailableLocales();

		final List countries = new ArrayList();

		for (final Locale element : available) {
			final String iso = element.getCountry();
			final String name = element.getDisplayCountry(locale);

			if (!EMPTY.equals(iso) && !EMPTY.equals(name)) {
				final LabelValue country = new LabelValue(name, iso);

				if (!countries.contains(country)) {
					countries.add(new LabelValue(name, iso));
				}
			}
		}

		Collections.sort(countries, new LabelValueComparator(locale));

		return countries;
	}

	/**
	 * Class to compare LabelValues using their labels with locale-sensitive
	 * behaviour.
	 */
	public class LabelValueComparator implements Comparator {

		/** The c. */
		private final Comparator c;

		/**
		 * Creates a new LabelValueComparator object.
		 * 
		 * @param locale
		 *            The Locale used for localized String comparison.
		 */
		public LabelValueComparator(final Locale locale) {
			this.c = Collator.getInstance(locale);
		}

		/**
		 * Compares the localized labels of two LabelValues.
		 * 
		 * @param o1
		 *            The first LabelValue to compare.
		 * @param o2
		 *            The second LabelValue to compare.
		 * 
		 * @return The value returned by comparing the localized labels.
		 */
		public final int compare(final Object o1, final Object o2) {
			final LabelValue lhs = (LabelValue) o1;
			final LabelValue rhs = (LabelValue) o2;

			return this.c.compare(lhs.getLabel(), rhs.getLabel());
		}
	}
}
