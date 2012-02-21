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
package alpha.portal.model;

/**
 * The Class AdornmentRules.
 */
public class AdornmentRules {

	/**
	 * Check the rules for every adornment.
	 * 
	 * @param aCard
	 *            the a card
	 * @param newAdornment
	 *            the new adornment
	 * @return True if no rule is violated, otherwise false.
	 */
	public static boolean applyRules(final AlphaCard aCard,
			final Adornment newAdornment) {

		final AdornmentType type = AdornmentType.fromName(newAdornment
				.getName());
		if ((type != null) && !type.validate(newAdornment.getValue()))
			return false;

		final Adornment oldAdornment = aCard.getAlphaCardDescriptor()
				.getAdornment(newAdornment.getName());

		final Adornment deleteAdornment = aCard.getAlphaCardDescriptor()
				.getAdornment(AdornmentType.Deleted.getName());

		if (deleteAdornment != null) {
			// check the adornment Deleted
			if (deleteAdornment.getValue().equals(
					AdornmentTypeDeleted.TRUE.value())
					&& (type != null) && (type != AdornmentType.Deleted))
				return false;
			else if (deleteAdornment.getValue().equals(
					AdornmentTypeDeleted.TRUE.value())
					&& (type == null))
				return false;
		}

		final boolean valid = true;

		// check the adornment Validity
		if (type == AdornmentType.Validity)
			return AdornmentRules
					.checkValidityRules(newAdornment, oldAdornment);

		// check the adornment Visibility
		if (type == AdornmentType.Visibility)
			return AdornmentRules.checkVisibilityRules(newAdornment,
					oldAdornment);

		// Payload sequenceNumber must not be editable
		if (newAdornment.getName().equals(
				AdornmentType.PayloadVersionNumber.getName()))
			return false;

		return valid;
	}

	/**
	 * Gets the new status of Data Provision. Should be called after adornment
	 * change.
	 * 
	 * @param aCard
	 *            The Alpha Card whose adornment has been changed.
	 * 
	 * @return The new status of Data Provision for this Alpha Card
	 */
	public static String getDataProvisionStatus(final AlphaCard aCard) {

		final Adornment validity = aCard.getAlphaCardDescriptor().getAdornment(
				AdornmentType.Validity.getName());
		final Adornment visibility = aCard.getAlphaCardDescriptor()
				.getAdornment(AdornmentType.Visibility.getName());
		final Payload payload = aCard.getPayload();

		// check the rules for Data Provision
		if (payload != null) {
			if ((validity != null)
					&& validity.getValue().equals(
							AdornmentTypeValidity.VALID.value())
					&& (visibility != null)
					&& visibility.getValue().equals(
							AdornmentTypeVisibility.PUBLIC.value()))
				return AdornmentTypeDataProvision.FULLFILLED.value();
			return AdornmentTypeDataProvision.INPROGRESS.value();

		}

		return AdornmentTypeDataProvision.OPEN.value();
	}

	/**
	 * Check the rules for Validity.
	 * 
	 * @param newAd
	 *            The new adornment.
	 * 
	 * @param oldAd
	 *            The old adornment.
	 * 
	 * @return True if no rule is violated, otherwise false.
	 */
	static boolean checkValidityRules(final Adornment newAd,
			final Adornment oldAd) {
		return !(newAd.getValue().equals(AdornmentTypeValidity.INVALID.value()) && oldAd
				.getValue().equals(AdornmentTypeValidity.VALID.value()));
	}

	/**
	 * Check the rules for Visibility.
	 * 
	 * @param newAd
	 *            The new adornment.
	 * 
	 * @param oldAd
	 *            The old adornment.
	 * 
	 * @return True if no rule is violated, otherwise false.
	 */
	static boolean checkVisibilityRules(final Adornment newAd,
			final Adornment oldAd) {
		return !(newAd.getValue().equals(
				AdornmentTypeVisibility.PRIVATE.value()) && oldAd.getValue()
				.equals(AdornmentTypeVisibility.PUBLIC.value()));
	}

}
