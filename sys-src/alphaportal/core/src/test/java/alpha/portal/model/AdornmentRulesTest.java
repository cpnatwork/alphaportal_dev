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

import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * The Class AdornmentRulesTest.
 */
public class AdornmentRulesTest {

	/** The test case id. */
	String testCaseId = "myTestCaseId";

	/** The test card id. */
	String testCardId = "myTestCardId";

	/** The test descriptor. */
	AlphaCardDescriptor testDescriptor;

	/** The test alpha card. */
	AlphaCard testAlphaCard;

	/** The new ad. */
	Adornment newAd;

	/** The old ad. */
	Adornment oldAd;

	/** The adornment list. */
	Set<Adornment> adornmentList;

	/** The payload. */
	Payload payload;

	/**
	 * Sets the up.
	 */
	@Before
	public void setUp() {
		this.testDescriptor = new AlphaCardDescriptor(this.testCaseId,
				this.testCardId);
		this.testDescriptor.setAdornment("1",
				AdornmentTypeVisibility.PRIVATE.value());
		this.testDescriptor.setAdornment("2",
				AdornmentTypeValidity.INVALID.value());
		this.testDescriptor.setAdornment("3",
				AdornmentTypeDeleted.FALSE.value());
		this.testAlphaCard = new AlphaCard(this.testDescriptor);
		this.adornmentList = this.testAlphaCard.getAlphaCardDescriptor()
				.getAllAdornments();
		this.payload = new Payload();
	}

	/**
	 * Test basics.
	 */
	@Test
	public void testBasics() {
		Adornment newAdornment = new Adornment("gibtsnicht");
		Assert.assertTrue(AdornmentRules.applyRules(this.testAlphaCard,
				newAdornment));

		newAdornment = new Adornment(AdornmentType.Visibility.getName());
		newAdornment.setValue("falscherwert");
		Assert.assertFalse(AdornmentRules.applyRules(this.testAlphaCard,
				newAdornment));
	}

	/**
	 * Test apply rules visibility.
	 */
	@Test
	public void testApplyRulesVisibility() {
		this.newAd = new Adornment(AdornmentType.Visibility.getName());
		this.newAd.setValue(AdornmentTypeVisibility.PRIVATE.value());
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(this.newAd);
		this.newAd = null;

		this.newAd = new Adornment(AdornmentType.Visibility.getName());
		this.newAd.setValue(AdornmentTypeVisibility.PRIVATE.value());
		Assert.assertTrue(AdornmentRules.applyRules(this.testAlphaCard,
				this.newAd));

		this.newAd.setValue(AdornmentTypeVisibility.PUBLIC.value());
		Assert.assertTrue(AdornmentRules.applyRules(this.testAlphaCard,
				this.newAd));
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(this.newAd);

		this.newAd.setValue(AdornmentTypeVisibility.PRIVATE.value());
		Assert.assertFalse(AdornmentRules.applyRules(this.testAlphaCard,
				this.newAd));

	}

	/**
	 * Test apply rules validity.
	 */
	@Test
	public void testApplyRulesValidity() {
		this.newAd = new Adornment(AdornmentType.Validity.getName());
		this.newAd.setValue(AdornmentTypeValidity.INVALID.value());
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(this.newAd);
		this.newAd = null;

		this.newAd = new Adornment(AdornmentType.Validity.getName());
		this.newAd.setValue(AdornmentTypeValidity.INVALID.value());
		Assert.assertTrue(AdornmentRules.applyRules(this.testAlphaCard,
				this.newAd));

		this.newAd.setValue(AdornmentTypeValidity.VALID.value());
		Assert.assertTrue(AdornmentRules.applyRules(this.testAlphaCard,
				this.newAd));
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(this.newAd);

		this.newAd.setValue(AdornmentTypeValidity.INVALID.value());
		Assert.assertFalse(AdornmentRules.applyRules(this.testAlphaCard,
				this.newAd));
	}

	/**
	 * Test data provision status.
	 */
	@Test
	public void testDataProvisionStatus() {
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(
				AdornmentType.Validity.name(),
				AdornmentTypeValidity.INVALID.name());
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(
				AdornmentType.Visibility.name(),
				AdornmentTypeVisibility.PRIVATE.name());
		Assert.assertEquals(AdornmentTypeDataProvision.OPEN.value(),
				AdornmentRules.getDataProvisionStatus(this.testAlphaCard));

		this.testAlphaCard.setPayload(new Payload());
		Assert.assertEquals(AdornmentTypeDataProvision.INPROGRESS.value(),
				AdornmentRules.getDataProvisionStatus(this.testAlphaCard));

		final Payload dummyPayload = new Payload("test", "test/mime");
		dummyPayload.setContent("some test data".getBytes());
		this.testAlphaCard.setPayload(dummyPayload);
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(
				AdornmentType.Validity.getName(),
				AdornmentTypeValidity.VALID.value());
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(
				AdornmentType.Visibility.getName(),
				AdornmentTypeVisibility.PUBLIC.value());
		Assert.assertEquals(AdornmentTypeDataProvision.FULLFILLED.value(),
				AdornmentRules.getDataProvisionStatus(this.testAlphaCard));

	}

	/**
	 * Test apply rules payload version.
	 */
	@Test
	public void testApplyRulesPayloadVersion() {
		final String oldVersion = "3";
		final String newVersion = "4";

		this.oldAd = new Adornment(AdornmentType.PayloadVersionNumber.getName());
		this.oldAd.setValue(oldVersion);

		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(this.oldAd);

		this.newAd = new Adornment(AdornmentType.PayloadVersionNumber.getName());
		this.newAd.setValue(newVersion);

		final boolean success = AdornmentRules.applyRules(this.testAlphaCard,
				this.newAd);

		Assert.assertFalse(success);
	}

	/**
	 * Test apply rules deleted.
	 */
	@Test
	public void testApplyRulesDeleted() {
		this.oldAd = new Adornment(AdornmentType.Visibility.getName());
		this.oldAd.setValue(AdornmentTypeVisibility.PUBLIC.value());

		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(this.oldAd);

		this.newAd = new Adornment(AdornmentType.Deleted.getName());
		this.newAd.setValue(AdornmentTypeDeleted.FALSE.value());
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(this.newAd);
		Assert.assertTrue(AdornmentRules.applyRules(this.testAlphaCard,
				this.newAd));

		this.newAd.setValue(AdornmentTypeDeleted.TRUE.value());
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(this.newAd);
		Assert.assertFalse(AdornmentRules.applyRules(this.testAlphaCard,
				this.oldAd));

		final Adornment newAdornment = new Adornment("gibtsnicht");
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(newAdornment);
		Assert.assertFalse(AdornmentRules.applyRules(this.testAlphaCard,
				newAdornment));

		this.newAd = new Adornment(AdornmentType.Deleted.getName());
		this.newAd.setValue(AdornmentTypeDeleted.TRUE.value());
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(this.newAd);
		Assert.assertTrue(AdornmentRules.applyRules(this.testAlphaCard,
				this.newAd));

		// newAd.setValue(AdornmentTypeDeleted.FALSE.value());
		// assertFalse(AdornmentRules.applyRules(testAlphaCard, newAd));
	}

	/**
	 * Test apply rules with wrong values.
	 */
	@Test
	public void testApplyRulesWithWrongValues() {
		final Adornment userAd = new Adornment();
		userAd.setName("myAdornment");

		boolean success = AdornmentRules.applyRules(this.testAlphaCard, userAd);
		Assert.assertTrue(success);

		final Adornment validity = new Adornment(
				AdornmentType.Visibility.getName());
		validity.setValue("wrongvalue");

		success = AdornmentRules.applyRules(this.testAlphaCard, validity);
		Assert.assertFalse(success);
	}

	/**
	 * Test provisional data.
	 */
	@Test
	public void testProvisionalData() {
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(
				AdornmentType.Visibility.getName(),
				AdornmentTypeVisibility.PRIVATE.value());
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(
				AdornmentType.Validity.getName(),
				AdornmentTypeValidity.INVALID.value());
		// no payload
		Assert.assertTrue(AdornmentRules
				.getDataProvisionStatus(this.testAlphaCard) == AdornmentTypeDataProvision.OPEN
				.value());

		// payload uploaded
		this.testAlphaCard.setPayload(this.payload);
		Assert.assertTrue(AdornmentRules
				.getDataProvisionStatus(this.testAlphaCard) == AdornmentTypeDataProvision.INPROGRESS
				.value());

		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(
				AdornmentType.Visibility.getName(),
				AdornmentTypeVisibility.PUBLIC.value());
		Assert.assertTrue(AdornmentRules
				.getDataProvisionStatus(this.testAlphaCard) == AdornmentTypeDataProvision.INPROGRESS
				.value());

		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(
				AdornmentType.Visibility.getName(),
				AdornmentTypeVisibility.PRIVATE.value());
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(
				AdornmentType.Validity.getName(),
				AdornmentTypeValidity.VALID.value());
		Assert.assertTrue(AdornmentRules
				.getDataProvisionStatus(this.testAlphaCard) == AdornmentTypeDataProvision.INPROGRESS
				.value());

		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(
				AdornmentType.Visibility.getName(),
				AdornmentTypeVisibility.PUBLIC.value());
		this.testAlphaCard.getAlphaCardDescriptor().setAdornment(
				AdornmentType.Validity.getName(),
				AdornmentTypeValidity.VALID.value());
		Assert.assertTrue(AdornmentRules
				.getDataProvisionStatus(this.testAlphaCard) == AdornmentTypeDataProvision.FULLFILLED
				.value());
	}
}
