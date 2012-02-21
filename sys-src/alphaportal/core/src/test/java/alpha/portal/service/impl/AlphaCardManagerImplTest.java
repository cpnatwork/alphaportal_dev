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

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.appfuse.service.impl.BaseManagerMockTestCase;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;
import org.jmock.Expectations;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import alpha.portal.dao.AlphaCardDao;
import alpha.portal.dao.AlphaCaseDao;
import alpha.portal.dao.PayloadDao;
import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.Payload;

/**
 * The Class AlphaCardManagerImplTest.
 */
public class AlphaCardManagerImplTest extends BaseManagerMockTestCase {

	/** The manager. */
	private AlphaCardManagerImpl manager = null;

	/** The payload manager. */
	private PayloadManagerImpl payloadManager = null;

	/** The dao. */
	private AlphaCardDao dao = null;

	/** The payload dao. */
	private PayloadDao payloadDao = null;

	/** The case dao. */
	private AlphaCaseDao caseDao = null;

	/**
	 * Sets the up.
	 */
	@Before
	public void setUp() {
		this.dao = this.context.mock(AlphaCardDao.class);
		this.caseDao = this.context.mock(AlphaCaseDao.class);
		this.manager = new AlphaCardManagerImpl(this.dao);

		this.payloadDao = this.context.mock(PayloadDao.class);
		this.payloadManager = new PayloadManagerImpl(this.payloadDao);

		this.manager.setPayloadManager(this.payloadManager);
		this.payloadManager.setAlphaCardManager(this.manager);
	}

	/**
	 * Tear down.
	 */
	@After
	public void tearDown() {
		this.manager = null;
	}

	/**
	 * Test create alpha card.
	 */
	@Test
	public void testCreateAlphaCard() {
		final String testCaseId = "myCaseId";
		final AlphaCard c = this.manager.createAlphaCard(testCaseId);
		Assert.assertTrue(c != null);
		Assert.assertNotNull(c.getAlphaCardDescriptor());
		Assert.assertNotNull(c.getAlphaCardIdentifier());
		Assert.assertEquals(testCaseId, c.getAlphaCardIdentifier().getCaseId());
		Assert.assertEquals(c.getAlphaCardIdentifier(), c
				.getAlphaCardDescriptor().getAlphaCardIdentifier());

		for (final AdornmentType type : AdornmentType.values()) {
			Assert.assertEquals(type.getValueDefault(), c
					.getAlphaCardDescriptor().getAdornment(type.getName())
					.getValue());
		}
	}

	/**
	 * Test get version.
	 */
	@Test
	public void testGetVersion() {
		final AlphaCardIdentifier id = new AlphaCardIdentifier("123", "321", 1L);
		final AlphaCard c = new AlphaCard();
		final AlphaCase aCase = new AlphaCase();
		c.setAlphaCardIdentifier(id);
		c.setAlphaCase(aCase);

		this.context.checking(new Expectations() {
			{
				this.one(AlphaCardManagerImplTest.this.dao).save(
						this.with(Expectations.same(c)));
				this.will(Expectations.returnValue(c));

			}
		});

		this.context.checking(new Expectations() {
			{
				this.one(AlphaCardManagerImplTest.this.caseDao).save(aCase);
				this.will(Expectations.returnValue(aCase));

			}
		});

		this.manager.setCaseManager(new CaseManagerImpl(this.caseDao));
		final AlphaCard c2 = this.manager.save(c);

		Assert.assertEquals(c, c2);

		this.context.checking(new Expectations() {
			{
				this.one(AlphaCardManagerImplTest.this.dao).getVersion(
						this.with(Expectations.same(id)));
				this.will(Expectations.returnValue(c));
			}
		});
		final AlphaCard c3 = this.manager.getVersion(id);
		Assert.assertEquals(c, c3);
	}

	/**
	 * Test get all versions.
	 */
	@Test
	public void testGetAllVersions() {
		final AlphaCardIdentifier id = new AlphaCardIdentifier("123", "321", 1L);
		final AlphaCard c = new AlphaCard();
		c.setAlphaCardIdentifier(id);

		final AlphaCardIdentifier id2 = new AlphaCardIdentifier("123", "321",
				2L);
		final AlphaCard c2 = new AlphaCard();
		c2.setAlphaCardIdentifier(id2);

		final List<AlphaCard> list = new LinkedList<AlphaCard>();
		list.add(c);
		list.add(c2);
		this.context.checking(new Expectations() {
			{
				this.one(AlphaCardManagerImplTest.this.dao).getAllVersions(
						this.with("123"));
				this.will(Expectations.returnValue(list));
			}
		});
		final List<AlphaCard> cards = this.manager.getAllVersions(id
				.getCaseId());
		Assert.assertTrue(cards.contains(c));
		Assert.assertTrue(cards.contains(c2));
		Assert.assertEquals(2, cards.size());
	}

	/**
	 * Test add save delete.
	 */
	@Test
	public void testAddSaveDelete() {

		final AlphaCardIdentifier id = new AlphaCardIdentifier("123", "321",
				null);
		final AlphaCard card = new AlphaCard();
		final AlphaCase aCase = new AlphaCase();
		card.setAlphaCardIdentifier(id);
		card.setAlphaCase(aCase);

		this.context.checking(new Expectations() {
			{
				this.one(AlphaCardManagerImplTest.this.dao).save(
						this.with(card));
				this.will(Expectations.returnValue(card));
			}
		});
		this.context.checking(new Expectations() {
			{
				this.one(AlphaCardManagerImplTest.this.caseDao).save(aCase);
				this.will(Expectations.returnValue(aCase));

			}
		});

		this.manager.setCaseManager(new CaseManagerImpl(this.caseDao));
		this.manager.addCard(card);

		this.context.checking(new Expectations() {
			{
				this.one(AlphaCardManagerImplTest.this.dao).exists(
						this.with(id));
				this.will(Expectations.returnValue(true));
			}
		});
		Assert.assertTrue(this.manager.exists(id));

		this.context.checking(new Expectations() {
			{
				this.one(AlphaCardManagerImplTest.this.dao).remove(
						this.with(card.getAlphaCardIdentifier()));
			}
		});
		this.manager.deleteCard(id.getCardId(), id.getCaseId());

		this.context.checking(new Expectations() {
			{
				this.one(AlphaCardManagerImplTest.this.dao).exists(
						this.with(id));
				this.will(Expectations.returnValue(false));
			}
		});
		Assert.assertFalse(this.manager.exists(id));
	}

	/**
	 * Test adornments.
	 */
	@Test
	public void testAdornments() {
		final AlphaCardIdentifier id = new AlphaCardIdentifier("123", "321",
				null);
		final AlphaCard card = new AlphaCard();
		final AlphaCase aCase = new AlphaCase();
		card.setAlphaCardIdentifier(id);
		card.setAlphaCase(aCase);

		final String name = "some name";
		final String value = "some stupid value";
		final Adornment adornment = new Adornment(name);
		adornment.setValue(value);

		final Expectations saveExpectation = new Expectations() {
			{
				this.exactly(3).of(AlphaCardManagerImplTest.this.dao)
						.save(this.with(card));
				this.will(Expectations.returnValue(card));
			}
		};
		final Expectations getCardExpectation = new Expectations() {
			{
				this.exactly(4).of(AlphaCardManagerImplTest.this.dao)
						.get(this.with(id));
				this.will(Expectations.returnValue(card));
			}
		};
		this.context.checking(new Expectations() {
			{
				this.exactly(2).of(AlphaCardManagerImplTest.this.caseDao)
						.save(aCase);
				this.will(Expectations.returnValue(aCase));

			}
		});

		this.manager.setCaseManager(new CaseManagerImpl(this.caseDao));

		this.context.checking(saveExpectation);
		this.manager.save(card);

		this.context.checking(getCardExpectation);
		final Adornment savedAdornment = this.manager.addAdornment(
				id.getCardId(), id.getCaseId(), adornment);
		Assert.assertEquals(adornment, savedAdornment);

		this.context.checking(getCardExpectation);
		final Adornment loadedAdornment = this.manager.getAdornment(
				id.getCardId(), id.getCaseId(), adornment.getName());
		Assert.assertEquals(adornment, loadedAdornment);

		this.context.checking(new Expectations() {
			{
				this.oneOf(AlphaCardManagerImplTest.this.dao).exists(
						this.with(id));
				this.will(Expectations.returnValue(true));
			}
		});
		Assert.assertTrue(this.manager.exists(id));

		this.context.checking(getCardExpectation);

		this.context.checking(new Expectations() {
			{
				this.oneOf(AlphaCardManagerImplTest.this.caseDao).save(
						this.with(card.getAlphaCase()));
				this.will(Expectations.returnValue(card.getAlphaCase()));
			}
		});
		this.manager.deleteAdornment(id.getCardId(), id.getCaseId(), name);
		Assert.assertNull(this.manager.get(id).getAlphaCardDescriptor()
				.getAdornment(value));

		final String fakeCardId = "1234-5678-90";
		final String fakeCaseId = "0987-6543-21";

		this.context.checking(new Expectations() {
			{
				this.oneOf(AlphaCardManagerImplTest.this.dao).get(
						this.with(new AlphaCardIdentifier(fakeCaseId,
								fakeCardId)));
				this.will(Expectations.returnValue(null));
			}
		});
		final Adornment nullTestAdornment = this.manager.getAdornment(
				fakeCardId, fakeCaseId, "Some Adornment");
		Assert.assertNull(nullTestAdornment);

		this.context.checking(new Expectations() {
			{
				this.oneOf(AlphaCardManagerImplTest.this.dao).get(
						this.with(new AlphaCardIdentifier(fakeCaseId,
								fakeCardId)));
				this.will(Expectations.returnValue(null));
			}
		});
		final Adornment unsavedAdornment = new Adornment("Foo");
		unsavedAdornment.setValue("Bar!");
		final Adornment unsavedAdornmentTest = this.manager.saveAdornment(
				fakeCardId, fakeCaseId, unsavedAdornment);
		Assert.assertTrue(unsavedAdornment.equals(unsavedAdornmentTest));

		this.context.checking(new Expectations() {
			{
				this.oneOf(AlphaCardManagerImplTest.this.dao).get(
						this.with(new AlphaCardIdentifier(fakeCaseId,
								fakeCardId)));
				this.will(Expectations.returnValue(null));
			}
		});
		this.manager.deleteAdornment(fakeCardId, fakeCaseId, "Non-Existing");
	}

	/**
	 * Test payload.
	 */
	@Test
	public void testPayload() {
		final AlphaCardIdentifier id = new AlphaCardIdentifier("123", "321",
				null);
		final AlphaCard card = new AlphaCard();
		final AlphaCase aCase = new AlphaCase();
		card.setAlphaCardIdentifier(id);
		card.setAlphaCase(aCase);

		final Payload payload = new Payload("somefile.txt", "text/plain");

		this.context.checking(new Expectations() {
			{
				this.exactly(2).of(AlphaCardManagerImplTest.this.dao)
						.save(this.with(card));
				this.will(Expectations.returnValue(card));
			}
		});
		this.context.checking(new Expectations() {
			{
				this.exactly(3).of(AlphaCardManagerImplTest.this.dao)
						.get(this.with(id));
				this.will(Expectations.returnValue(card));
			}
		});

		this.context.checking(new Expectations() {
			{
				this.exactly(1).of(AlphaCardManagerImplTest.this.payloadDao)
						.save(this.with(payload));
				this.will(Expectations.returnValue(payload));
			}
		});

		this.context.checking(new Expectations() {
			{
				this.exactly(1).of(AlphaCardManagerImplTest.this.dao)
						.exists(this.with(id));
				this.will(Expectations.returnValue(true));
			}
		});
		this.context.checking(new Expectations() {
			{
				this.one(AlphaCardManagerImplTest.this.caseDao).save(aCase);
				this.will(Expectations.returnValue(aCase));

			}
		});

		this.manager.setCaseManager(new CaseManagerImpl(this.caseDao));

		this.manager.save(card);

		this.context.checking(new Expectations() {
			{
				this.oneOf(AlphaCardManagerImplTest.this.caseDao).save(
						this.with(card.getAlphaCase()));
				this.will(Expectations.returnValue(card.getAlphaCase()));
			}
		});
		final Payload savedPayload = this.manager.setPayload(id.getCardId(),
				id.getCaseId(), payload);
		Assert.assertNotNull(payload.getPayloadIdentifier());

		this.manager.getPayload(id.getCardId(), id.getCaseId());

		Assert.assertEquals(savedPayload, this.manager.get(id).getPayload());

		final String fakeCardId = "123";
		final String fakeCaseId = "456";

		this.context.checking(new Expectations() {
			{
				this.oneOf(AlphaCardManagerImplTest.this.dao).get(
						this.with(new AlphaCardIdentifier(fakeCaseId,
								fakeCardId)));
				this.will(Expectations.returnValue(null));
			}
		});
		Assert.assertNull(this.manager.getPayload(fakeCardId, fakeCaseId));

		this.context.checking(new Expectations() {
			{
				this.oneOf(AlphaCardManagerImplTest.this.dao).get(
						this.with(new AlphaCardIdentifier(fakeCaseId,
								fakeCardId)));
				this.will(Expectations.returnValue(null));
			}
		});
		Payload testPayload = new Payload("Load", "mime");
		testPayload = this.manager.setPayload(fakeCardId, fakeCaseId,
				testPayload);
		Assert.assertNull(testPayload);

		this.context.checking(new Expectations() {
			{
				this.oneOf(AlphaCardManagerImplTest.this.dao).get(
						this.with(new AlphaCardIdentifier(fakeCaseId,
								fakeCardId)));
				this.will(Expectations.returnValue(new AlphaCard()));
			}
		});
		testPayload = null;
		Assert.assertNull(this.manager.setPayload(fakeCardId, fakeCaseId,
				testPayload));
	}

	/**
	 * Test list alpha cards by criterion.
	 */
	@Test
	public void testListAlphaCardsByCriterion() {
		final String caseId = "123";
		final Criterion crit = Restrictions.eq("gibtsnicht", "");
		final List<AlphaCard> list = new LinkedList<AlphaCard>();
		this.context.checking(new Expectations() {
			{
				this.one(AlphaCardManagerImplTest.this.dao)
						.listAlphaCardsByCriterion(
								this.with(Expectations.equal(caseId)),
								this.with(Expectations
										.equal(new Criterion[] { crit })));
				this.will(Expectations.returnValue(list));
			}
		});
		this.manager.listAlphaCardsByCriterion(caseId, crit);

		Assert.assertNull(this.manager.listAlphaCardsByCriterion(null, crit));
		Assert.assertNull(this.manager.listAlphaCardsByCriterion("", crit));
	}

	/**
	 * Test criterion.
	 */
	@Test
	public void testCriterion() {
		Assert.assertEquals(
				Restrictions.and(AlphaCardManagerImpl._CONTRIBUTOR,
						Restrictions.ne("ad.value", "123")).toString(),
				AlphaCardManagerImpl.getContributorCriterionOthers("123")
						.toString());
		Assert.assertEquals(
				Restrictions.and(AlphaCardManagerImpl._CONTRIBUTOR,
						Restrictions.eq("ad.value", "123")).toString(),
				AlphaCardManagerImpl.getContributorCriterionOwn("123")
						.toString());
		Assert.assertEquals(
				Restrictions.and(
						AlphaCardManagerImpl._CONTRIBUTORROLE,
						Restrictions.not(Restrictions.in("ad.value",
								new String[] { "role1" }))).toString(),
				AlphaCardManagerImpl.getContributorRoleCriterionOthers("role1")
						.toString());
		Assert.assertEquals(
				Restrictions.and(AlphaCardManagerImpl._CONTRIBUTORROLE,
						Restrictions.in("ad.value", new String[] { "role1" }))
						.toString(), AlphaCardManagerImpl
						.getContributorRoleCriterionOwn("role1").toString());
	}

	/**
	 * Test list dash board cards.
	 */
	@Test
	public void testListDashBoardCards() {
		final AlphaCase testAlphaCase = new AlphaCase();
		testAlphaCase.setCaseId("01234-567-890");
		testAlphaCase.setName("Test AlphaCase");
		final List<AlphaCase> caseList = new ArrayList<AlphaCase>();
		caseList.add(testAlphaCase);

		final String[] caseIDs = new String[1];
		caseIDs[0] = caseList.get(0).getCaseId();

		final List<AlphaCard> retVal = new ArrayList<AlphaCard>();

		this.context.checking(new Expectations() {
			{
				this.one(AlphaCardManagerImplTest.this.dao)
						.listDashBoardAlphaCards(
								this.with(Expectations.equal(caseIDs)));
				this.will(Expectations.returnValue(retVal));
			}
		});
		final List<AlphaCard> dutReturn = this.manager
				.listDashBoardCards(caseList);

		Assert.assertTrue(dutReturn.equals(retVal));
	}
}
