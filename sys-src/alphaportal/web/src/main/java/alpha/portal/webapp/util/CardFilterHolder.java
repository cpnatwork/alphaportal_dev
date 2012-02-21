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

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * The Class CardFilterHolder.
 */
public class CardFilterHolder {

	/** The data provision. */
	private CardFilterDataProvision dataProvision;

	/** The contributor role. */
	private CardFilterContributorRole contributorRole;

	/** The contributor. */
	private CardFilterContributor contributor;

	/** The deleted. */
	private CardFilterDeleted deleted;

	/**
	 * Gets the data provision.
	 * 
	 * @return the dataProvision
	 */
	public CardFilterDataProvision getDataProvision() {
		return this.dataProvision;
	}

	/**
	 * Sets the data provision.
	 * 
	 * @param dataProvision
	 *            the dataProvision to set
	 */
	public void setDataProvision(final CardFilterDataProvision dataProvision) {
		this.dataProvision = dataProvision;
	}

	/**
	 * Gets the contributor role.
	 * 
	 * @return the contributorRole
	 */
	public CardFilterContributorRole getContributorRole() {
		return this.contributorRole;
	}

	/**
	 * Sets the contributor role.
	 * 
	 * @param contributorRole
	 *            the contributorRole to set
	 */
	public void setContributorRole(
			final CardFilterContributorRole contributorRole) {
		this.contributorRole = contributorRole;
	}

	/**
	 * Gets the contributor.
	 * 
	 * @return the contributor
	 */
	public CardFilterContributor getContributor() {
		return this.contributor;
	}

	/**
	 * Sets the contributor.
	 * 
	 * @param contributor
	 *            the contributor to set
	 */
	public void setContributor(final CardFilterContributor contributor) {
		this.contributor = contributor;
	}

	/**
	 * Gets the show deleted.
	 * 
	 * @return Show Deleted?
	 */
	public CardFilterDeleted getShowDeleted() {
		return this.deleted;
	}

	/**
	 * Sets the show deleted.
	 * 
	 * @param deleted
	 *            Set: show deleted?
	 */
	public void setShowDeleted(final CardFilterDeleted deleted) {
		this.deleted = deleted;
	}

	/**
	 * Merge filters with session.
	 * 
	 * @param request
	 *            the request
	 * @param response
	 *            the response
	 */
	public void mergeFiltersWithSession(final HttpServletRequest request,
			final HttpServletResponse response) {
		String cookieValDataProvision = null;
		String cookieValContributorRole = null;
		String cookieValContributor = null;
		String cookieValDeleted = null;

		final Cookie[] cookies = request.getCookies();
		if (cookies != null) {
			for (final Cookie thisCookie : cookies) {
				if (thisCookie.getName().equals("aCardFilterDataProvision")) {
					cookieValDataProvision = thisCookie.getValue();
				} else if (thisCookie.getName().equals(
						"aCardFilterContributorRole")) {
					cookieValContributorRole = thisCookie.getValue();
				} else if (thisCookie.getName()
						.equals("aCardFilterContributor")) {
					cookieValContributor = thisCookie.getValue();
				} else if (thisCookie.getName().equals("aCardFilterDeleted")) {
					cookieValDeleted = thisCookie.getValue();
				}
			}
		}

		boolean isOk;

		if (this.dataProvision == null) {
			if (cookieValDataProvision != null) {
				isOk = false;
				for (final CardFilterDataProvision compare : CardFilterDataProvision
						.values()) {
					if (cookieValDataProvision.equals(compare.name())) {
						isOk = true;
						this.dataProvision = compare;
						break;
					}
				}

				if (!isOk) {
					this.dataProvision = CardFilterDataProvision.ALL;
				}
			} else if (cookieValDataProvision == null) {
				this.dataProvision = CardFilterDataProvision.ALL;
			}
		}

		if (this.contributorRole == null) {
			if (cookieValContributorRole != null) {
				isOk = false;
				for (final CardFilterContributorRole compare : CardFilterContributorRole
						.values()) {
					if (cookieValContributorRole.equals(compare.name())) {
						isOk = true;
						this.contributorRole = compare;
						break;
					}
				}

				if (!isOk) {
					this.contributorRole = CardFilterContributorRole.ALL;
				}
			} else if (cookieValContributorRole == null) {
				this.contributorRole = CardFilterContributorRole.ALL;
			}
		}

		if (this.contributor == null) {
			if (cookieValContributor != null) {
				isOk = false;
				for (final CardFilterContributor compare : CardFilterContributor
						.values()) {
					if (cookieValContributor.equals(compare.name())) {
						isOk = true;
						this.contributor = compare;
						break;
					}
				}

				if (!isOk) {
					this.contributor = CardFilterContributor.ALL;
				}
			} else if (cookieValContributor == null) {
				this.contributor = CardFilterContributor.ALL;
			}
		}

		if (this.deleted == null) {
			if (cookieValDeleted != null) {
				isOk = false;
				for (final CardFilterDeleted compare : CardFilterDeleted
						.values()) {
					if (cookieValDeleted.equals(compare.name())) {
						isOk = true;
						this.deleted = compare;
						break;
					}
				}

				if (!isOk) {
					this.deleted = CardFilterDeleted.NOTDELETED;
				}
			} else if (cookieValDeleted == null) {
				this.deleted = CardFilterDeleted.NOTDELETED;
			}
		}

		Cookie cookie = new Cookie("aCardFilterDataProvision",
				this.dataProvision.name());
		cookie.setSecure(false);
		cookie.setPath("/");
		cookie.setMaxAge(3600 * 24 * 30);
		response.addCookie(cookie);

		cookie = new Cookie("aCardFilterContributorRole",
				this.contributorRole.name());
		cookie.setSecure(false);
		cookie.setPath("/");
		cookie.setMaxAge(3600 * 24 * 30);
		response.addCookie(cookie);

		cookie = new Cookie("aCardFilterContributor", this.contributor.name());
		cookie.setSecure(false);
		cookie.setPath("/");
		cookie.setMaxAge(3600 * 24 * 30);
		response.addCookie(cookie);

		cookie = new Cookie("aCardFilterDeleted", this.deleted.name());
		cookie.setSecure(false);
		cookie.setPath("/");
		cookie.setMaxAge(3600 * 24 * 30);
		response.addCookie(cookie);

		return;
	}
}
