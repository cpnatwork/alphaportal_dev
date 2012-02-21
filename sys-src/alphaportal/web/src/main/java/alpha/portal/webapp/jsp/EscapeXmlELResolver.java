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
package alpha.portal.webapp.jsp;

import java.beans.FeatureDescriptor;
import java.util.Iterator;

import javax.el.ELContext;
import javax.el.ELException;
import javax.el.ELResolver;
import javax.el.PropertyNotFoundException;
import javax.el.PropertyNotWritableException;

/**
 * The Class EscapeXmlELResolver.
 * 
 * {@link ELResolver} which escapes XML in String values.
 */
public class EscapeXmlELResolver extends ELResolver {

	/** The original resolver. */
	private ELResolver originalResolver;

	/** The getting value. */
	private final ThreadLocal<Boolean> gettingValue = new ThreadLocal<Boolean>() {
		@Override
		protected Boolean initialValue() {
			return Boolean.FALSE;
		}
	};

	/**
	 * Gets the original resolver.
	 * 
	 * @param context
	 *            the context
	 * @return the original resolver
	 */
	private ELResolver getOriginalResolver(final ELContext context) {
		if (this.originalResolver == null) {
			this.originalResolver = context.getELResolver();
		}
		return this.originalResolver;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.el.ELResolver#getCommonPropertyType(javax.el.ELContext,
	 * java.lang.Object)
	 */
	@Override
	public Class<?> getCommonPropertyType(final ELContext context,
			final Object base) {
		return this.getOriginalResolver(context).getCommonPropertyType(context,
				base);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.el.ELResolver#getFeatureDescriptors(javax.el.ELContext,
	 * java.lang.Object)
	 */
	@Override
	public Iterator<FeatureDescriptor> getFeatureDescriptors(
			final ELContext context, final Object base) {
		return this.getOriginalResolver(context).getFeatureDescriptors(context,
				base);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.el.ELResolver#getType(javax.el.ELContext, java.lang.Object,
	 * java.lang.Object)
	 */
	@Override
	public Class<?> getType(final ELContext context, final Object base,
			final Object property) throws NullPointerException,
			PropertyNotFoundException, ELException {
		return this.getOriginalResolver(context).getType(context, base,
				property);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.el.ELResolver#getValue(javax.el.ELContext, java.lang.Object,
	 * java.lang.Object)
	 */
	@Override
	public Object getValue(final ELContext context, final Object base,
			final Object property) throws NullPointerException,
			PropertyNotFoundException, ELException {
		if (this.gettingValue.get())
			return null;

		// This resolver is in the original resolver chain. When this resolver
		// invokes the original resolver chain, set a flag so when execution
		// reaches this resolver, act like this resolver is not in the chain.
		this.gettingValue.set(true);
		Object value = this.getOriginalResolver(context).getValue(context,
				base, property);
		this.gettingValue.set(false);

		if (value instanceof String) {
			value = EscapeXml.escape((String) value);
		}
		return value;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.el.ELResolver#isReadOnly(javax.el.ELContext, java.lang.Object,
	 * java.lang.Object)
	 */
	@Override
	public boolean isReadOnly(final ELContext context, final Object base,
			final Object property) throws NullPointerException,
			PropertyNotFoundException, ELException {
		return this.getOriginalResolver(context).isReadOnly(context, base,
				property);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.el.ELResolver#setValue(javax.el.ELContext, java.lang.Object,
	 * java.lang.Object, java.lang.Object)
	 */
	@Override
	public void setValue(final ELContext context, final Object base,
			final Object property, final Object value)
			throws NullPointerException, PropertyNotFoundException,
			PropertyNotWritableException, ELException {
		this.getOriginalResolver(context).setValue(context, base, property,
				value);
	}
}
