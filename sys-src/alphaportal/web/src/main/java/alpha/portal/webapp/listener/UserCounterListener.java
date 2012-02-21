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

import java.util.LinkedHashSet;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.http.HttpSessionAttributeListener;
import javax.servlet.http.HttpSessionBindingEvent;

import org.appfuse.model.User;
import org.springframework.security.authentication.AuthenticationTrustResolver;
import org.springframework.security.authentication.AuthenticationTrustResolverImpl;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.context.HttpSessionContextIntegrationFilter;

/**
 * UserCounterListener class used to count the current number of active users
 * for the applications. Does this by counting how many user objects are stuffed
 * into the session. It also grabs these users and exposes them in the servlet
 * context.
 * 
 * @author <a href="mailto:matt@raibledesigns.com">Matt Raible</a>
 */
public class UserCounterListener implements ServletContextListener,
		HttpSessionAttributeListener {

	/** Name of user counter variable. */
	public static final String COUNT_KEY = "userCounter";

	/** Name of users Set in the ServletContext. */
	public static final String USERS_KEY = "userNames";
	/**
	 * The default event we're looking to trap.
	 */
	public static final String EVENT_KEY = HttpSessionContextIntegrationFilter.SPRING_SECURITY_CONTEXT_KEY;

	/** The servlet context. */
	private transient ServletContext servletContext;

	/** The counter. */
	private int counter;

	/** The users. */
	private Set<User> users;

	/**
	 * Initialize the context.
	 * 
	 * @param sce
	 *            the event
	 */
	public synchronized void contextInitialized(final ServletContextEvent sce) {
		this.servletContext = sce.getServletContext();
		this.servletContext.setAttribute((UserCounterListener.COUNT_KEY),
				Integer.toString(this.counter));
	}

	/**
	 * Set the servletContext, users and counter to null.
	 * 
	 * @param event
	 *            The servletContextEvent
	 */
	public synchronized void contextDestroyed(final ServletContextEvent event) {
		this.servletContext = null;
		this.users = null;
		this.counter = 0;
	}

	/**
	 * Increment user counter.
	 */
	synchronized void incrementUserCounter() {
		this.counter = Integer.parseInt((String) this.servletContext
				.getAttribute(UserCounterListener.COUNT_KEY));
		this.counter++;
		this.servletContext.setAttribute(UserCounterListener.COUNT_KEY,
				Integer.toString(this.counter));
	}

	/**
	 * Decrement user counter.
	 */
	synchronized void decrementUserCounter() {
		int counter = Integer.parseInt((String) this.servletContext
				.getAttribute(UserCounterListener.COUNT_KEY));
		counter--;

		if (counter < 0) {
			counter = 0;
		}

		this.servletContext.setAttribute(UserCounterListener.COUNT_KEY,
				Integer.toString(counter));
	}

	/**
	 * Adds the username.
	 * 
	 * @param user
	 *            the user
	 */
	@SuppressWarnings("unchecked")
	synchronized void addUsername(final User user) {
		this.users = (Set<User>) this.servletContext
				.getAttribute(UserCounterListener.USERS_KEY);

		if (this.users == null) {
			this.users = new LinkedHashSet<User>();
		}

		if (!this.users.contains(user)) {
			this.users.add(user);
			this.servletContext.setAttribute(UserCounterListener.USERS_KEY,
					this.users);
			this.incrementUserCounter();
		}
	}

	/**
	 * Removes the username.
	 * 
	 * @param user
	 *            the user
	 */
	@SuppressWarnings("unchecked")
	synchronized void removeUsername(final User user) {
		this.users = (Set<User>) this.servletContext
				.getAttribute(UserCounterListener.USERS_KEY);

		if (this.users != null) {
			this.users.remove(user);
		}

		this.servletContext.setAttribute(UserCounterListener.USERS_KEY,
				this.users);
		this.decrementUserCounter();
	}

	/**
	 * This method is designed to catch when user's login and record their name.
	 * 
	 * @param event
	 *            the event to process
	 * @see javax.servlet.http.HttpSessionAttributeListener#attributeAdded(javax.servlet.http.HttpSessionBindingEvent)
	 */
	public void attributeAdded(final HttpSessionBindingEvent event) {
		if (event.getName().equals(UserCounterListener.EVENT_KEY)
				&& !this.isAnonymous()) {
			final SecurityContext securityContext = (SecurityContext) event
					.getValue();
			if (securityContext.getAuthentication().getPrincipal() instanceof User) {
				final User user = (User) securityContext.getAuthentication()
						.getPrincipal();
				this.addUsername(user);
			}
		}
	}

	/**
	 * Checks if is anonymous.
	 * 
	 * @return true, if is anonymous
	 */
	private boolean isAnonymous() {
		final AuthenticationTrustResolver resolver = new AuthenticationTrustResolverImpl();
		final SecurityContext ctx = SecurityContextHolder.getContext();
		if (ctx != null) {
			final Authentication auth = ctx.getAuthentication();
			return resolver.isAnonymous(auth);
		}
		return true;
	}

	/**
	 * When user's logout, remove their name from the hashMap.
	 * 
	 * @param event
	 *            the session binding event
	 * @see javax.servlet.http.HttpSessionAttributeListener#attributeRemoved(javax.servlet.http.HttpSessionBindingEvent)
	 */
	public void attributeRemoved(final HttpSessionBindingEvent event) {
		if (event.getName().equals(UserCounterListener.EVENT_KEY)
				&& !this.isAnonymous()) {
			final SecurityContext securityContext = (SecurityContext) event
					.getValue();
			final Authentication auth = securityContext.getAuthentication();
			if ((auth != null) && (auth.getPrincipal() instanceof User)) {
				final User user = (User) auth.getPrincipal();
				this.removeUsername(user);
			}
		}
	}

	/**
	 * Needed for Acegi Security 1.0, as it adds an anonymous user to the
	 * session and then replaces it after authentication.
	 * http://forum.springframework.org/showthread.php?p=63593
	 * 
	 * @param event
	 *            the session binding event
	 * @see javax.servlet.http.HttpSessionAttributeListener#attributeReplaced(javax.servlet.http.HttpSessionBindingEvent)
	 */
	public void attributeReplaced(final HttpSessionBindingEvent event) {
		if (event.getName().equals(UserCounterListener.EVENT_KEY)
				&& !this.isAnonymous()) {
			final SecurityContext securityContext = (SecurityContext) event
					.getValue();
			if ((securityContext.getAuthentication() != null)
					&& (securityContext.getAuthentication().getPrincipal() instanceof User)) {
				final User user = (User) securityContext.getAuthentication()
						.getPrincipal();
				this.addUsername(user);
			}
		}
	}
}
