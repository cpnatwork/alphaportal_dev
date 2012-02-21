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
package alpha.portal.service.impl;

import java.util.List;

import org.appfuse.service.impl.GenericManagerImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import alpha.portal.dao.PayloadDao;
import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentRules;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.Payload;
import alpha.portal.model.PayloadIdentifier;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.PayloadManager;

/**
 * implements the interface PayloadManager.
 * 
 * @see PayloadManager Payload Manager
 */
@Service("payloadManager")
public class PayloadManagerImpl extends
		GenericManagerImpl<Payload, PayloadIdentifier> implements
		PayloadManager {

	/** The alpha card manager. */
	@Autowired
	private AlphaCardManager alphaCardManager;

	/** The payload dao. */
	private final PayloadDao payloadDao;

	/**
	 * Sets the alpha card manager.
	 * 
	 * @param manager
	 *            the new alpha card manager
	 */
	public void setAlphaCardManager(final AlphaCardManager manager) {
		this.alphaCardManager = manager;
	}

	/**
	 * Instantiates a new payload manager impl.
	 * 
	 * @param payloadDao
	 *            the payload dao
	 */
	@Autowired
	public PayloadManagerImpl(final PayloadDao payloadDao) {
		super(payloadDao);
		this.payloadDao = payloadDao;
	}

	/**
	 * Save new payload.
	 * 
	 * @param payload
	 *            the payload
	 * @param card
	 *            the card
	 * @return the payload
	 * @see alpha.portal.service.PayloadManager#saveNewPayload(alpha.portal.model.Payload,
	 *      alpha.portal.model.AlphaCard)
	 */
	public Payload saveNewPayload(Payload payload, final AlphaCard card) {
		if (!this.alphaCardManager.exists(card.getAlphaCardIdentifier()))
			throw new IllegalArgumentException("Card does not exist!");

		// take existing payloadId if set - else dao.save() will generate one
		if (card.getPayload() != null) {
			payload.getPayloadIdentifier().setPayloadId(
					card.getPayload().getPayloadIdentifier().getPayloadId());
		}

		// set payload now for AdornmentRules.getDataProvisionStatus
		card.setPayload(payload);

		// increment adornment "Payload Version"
		Adornment adPayloadVersion = card.getAlphaCardDescriptor()
				.getAdornment(AdornmentType.PayloadVersionNumber.getName());
		if (adPayloadVersion == null) {
			adPayloadVersion = new Adornment(
					AdornmentType.PayloadVersionNumber.getName());
			adPayloadVersion.setValue("0");
		}
		int currentPayloadVersion = 0;
		try {
			currentPayloadVersion = Integer.parseInt(adPayloadVersion
					.getValue());
		} catch (final NumberFormatException e) {
			throw new IllegalArgumentException(adPayloadVersion.getValue());
		}
		adPayloadVersion.setValue(Integer.toString(++currentPayloadVersion));
		card.getAlphaCardDescriptor().setAdornment(adPayloadVersion);

		// update DataProvision
		card.getAlphaCardDescriptor().setAdornment(
				AdornmentType.DataProvision.getName(),
				AdornmentRules.getDataProvisionStatus(card));

		// save payload first so we have a valid identifier
		payload = super.save(payload);
		// then save its connection to the AlphaCard
		card.setPayload(payload);
		/* card = */this.alphaCardManager.save(card);
		return payload;
	}

	/**
	 * Gets the all versions.
	 * 
	 * @param payload
	 *            the payload
	 * @return the all versions
	 * @see alpha.portal.service.PayloadManager#getAllVersions()
	 */
	public List<Payload> getAllVersions(final Payload payload) {
		return this.payloadDao.getAllVersions(payload);
	}

	/**
	 * Gets the version.
	 * 
	 * @param id
	 *            the id
	 * @return the version
	 * @see alpha.portal.service.PayloadManager#getVersion(int)
	 */
	public Payload getVersion(final PayloadIdentifier id) {
		return this.payloadDao.getVersion(id);
	}

}
