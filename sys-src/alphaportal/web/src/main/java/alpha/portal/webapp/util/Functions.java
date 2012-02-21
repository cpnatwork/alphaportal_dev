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

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AdornmentTypeDataProvision;
import alpha.portal.model.AdornmentTypeDeleted;
import alpha.portal.model.AlphaCard;

/**
 * This class implements some function which can be used from jsp-Pages by the
 * JSP Expression language.
 * 
 * @author uz87odun
 */

public class Functions {

	/**
	 * Get an adornment's value of the given card by the adornment's name.
	 * 
	 * @param adornmentName
	 *            the name of the adornment
	 * @param card
	 *            the card to use
	 * @return the value of the adornment or an empty String if none found
	 */
	public static String adornmentValueFromCard(final String adornmentName,
			final AlphaCard card) {
		if ((card != null) && (card.getAlphaCardDescriptor() != null)) {
			final Adornment adornment = card.getAlphaCardDescriptor()
					.getAdornment(adornmentName);
			if ((adornment != null) && (adornment.getValue() != null))
				return adornment.getValue();
		}
		return "";
	}

	/**
	 * Get the data-provision-sate from card.
	 * 
	 * @param card
	 *            the card to use
	 * @return the data-provision state of this card
	 */
	public static String dataProvisionFromCard(final AlphaCard card) {
		final String adornmentValue = Functions.adornmentValueFromCard(
				AdornmentType.DataProvision.getName(), card);
		if (!StringUtils.isBlank(adornmentValue)) {
			final AdornmentTypeDataProvision dataProvision = AdornmentTypeDataProvision
					.fromValue(adornmentValue);
			if (dataProvision != null)
				return dataProvision.name().toLowerCase();
		}
		return "";
	}

	/**
	 * Checks if is card marked as deleted.
	 * 
	 * @param card
	 *            the card
	 * @return true, if is card marked as deleted
	 */
	public static boolean isCardMarkedAsDeleted(final AlphaCard card) {
		final String adornmentValue = Functions.adornmentValueFromCard(
				AdornmentType.Deleted.getName(), card);
		if (!StringUtils.isBlank(adornmentValue)) {
			final AdornmentTypeDeleted adornmentType = AdornmentTypeDeleted
					.fromValue(adornmentValue);
			return adornmentType.equals(AdornmentTypeDeleted.TRUE);
		}
		return false;
	}

	/**
	 * Checks if is user contributor.
	 * 
	 * @param userId
	 *            the user id
	 * @param card
	 *            the card
	 * @return true, if is user contributor
	 */
	public static boolean isUserContributor(final Long userId,
			final AlphaCard card) {
		if ((card != null) && (card.getAlphaCardDescriptor() != null)) {
			final Long contributor = card.getAlphaCardDescriptor()
					.getContributor();
			if (contributor != null)
				return contributor.equals(userId);
		}
		return false;
	}

	/**
	 * Contains.
	 * 
	 * @param list
	 *            the list
	 * @param o
	 *            the o
	 * @return true, if successful
	 */
	@SuppressWarnings("unchecked")
	public static boolean contains(final String[] list, final String o) {
		return ArrayUtils.contains(list, o);
	}
}
