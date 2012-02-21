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
package alpha.portal.webapp.listener;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.appfuse.Constants;
import org.appfuse.service.LookupManager;
import org.compass.gps.CompassGps;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.authentication.RememberMeAuthenticationProvider;
import org.springframework.security.authentication.encoding.PasswordEncoder;
import org.springframework.web.context.support.WebApplicationContextUtils;

/**
 * <p>
 * StartupListener class used to initialize and database settings and populate
 * any application-wide drop-downs.
 * <p/>
 * <p>
 * Keep in mind that this listener is executed outside of
 * OpenSessionInViewFilter, so if you're using Hibernate you'll have to
 * explicitly initialize all loaded data at the GenericDao or service level to
 * avoid LazyInitializationException. Hibernate.initialize() works well for
 * doing this.
 * 
 * @author <a href="mailto:matt@raibledesigns.com">Matt Raible</a>
 */
public class StartupListener implements ServletContextListener {

	/** The Constant log. */
	private static final Log log = LogFactory.getLog(StartupListener.class);

	/**
	 * {@inheritDoc}
	 */
	@SuppressWarnings("unchecked")
	public void contextInitialized(final ServletContextEvent event) {
		StartupListener.log.debug("Initializing context...");

		final ServletContext context = event.getServletContext();

		// Orion starts Servlets before Listeners, so check if the config
		// object already exists
		Map<String, Object> config = (HashMap<String, Object>) context
				.getAttribute(Constants.CONFIG);

		if (config == null) {
			config = new HashMap<String, Object>();
		}

		if (context.getInitParameter(Constants.CSS_THEME) != null) {
			config.put(Constants.CSS_THEME,
					context.getInitParameter(Constants.CSS_THEME));
		}

		final ApplicationContext ctx = WebApplicationContextUtils
				.getRequiredWebApplicationContext(context);

		/*
		 * String[] beans = ctx.getBeanDefinitionNames(); for (String bean :
		 * beans) { log.debug(bean); }
		 */

		PasswordEncoder passwordEncoder = null;
		try {
			final ProviderManager provider = (ProviderManager) ctx
					.getBean("org.springframework.security.authentication.ProviderManager#0");
			for (final Object o : provider.getProviders()) {
				final AuthenticationProvider p = (AuthenticationProvider) o;
				if (p instanceof RememberMeAuthenticationProvider) {
					config.put("rememberMeEnabled", Boolean.TRUE);
				} else if (ctx.getBean("passwordEncoder") != null) {
					passwordEncoder = (PasswordEncoder) ctx
							.getBean("passwordEncoder");
				}
			}
		} catch (final NoSuchBeanDefinitionException n) {
			StartupListener.log
					.debug("authenticationManager bean not found, assuming test and ignoring...");
			// ignore, should only happen when testing
		}

		context.setAttribute(Constants.CONFIG, config);

		// output the retrieved values for the Init and Context Parameters
		if (StartupListener.log.isDebugEnabled()) {
			StartupListener.log.debug("Remember Me Enabled? "
					+ config.get("rememberMeEnabled"));
			if (passwordEncoder != null) {
				StartupListener.log.debug("Password Encoder: "
						+ passwordEncoder.getClass().getSimpleName());
			}
			StartupListener.log.debug("Populating drop-downs...");
		}

		StartupListener.setupContext(context);
	}

	/**
	 * This method uses the LookupManager to lookup available roles from the
	 * data layer.
	 * 
	 * @param context
	 *            The servlet context
	 */
	public static void setupContext(final ServletContext context) {
		final ApplicationContext ctx = WebApplicationContextUtils
				.getRequiredWebApplicationContext(context);
		final LookupManager mgr = (LookupManager) ctx.getBean("lookupManager");

		// get list of possible roles
		context.setAttribute(Constants.AVAILABLE_ROLES, mgr.getAllRoles());
		StartupListener.log.debug("Drop-down initialization complete [OK]");

		final CompassGps compassGps = ctx.getBean(CompassGps.class);
		compassGps.index();
	}

	/**
	 * Shutdown servlet context (currently a no-op method).
	 * 
	 * @param servletContextEvent
	 *            The servlet context event
	 */
	public void contextDestroyed(final ServletContextEvent servletContextEvent) {
		// LogFactory.release(Thread.currentThread().getContextClassLoader());
		// Commented out the above call to avoid warning when SLF4J in
		// classpath.
		// WARN: The method class
		// org.apache.commons.logging.impl.SLF4JLogFactory#release() was
		// invoked.
		// WARN: Please see http://www.slf4j.org/codes.html for an explanation.
	}
}
