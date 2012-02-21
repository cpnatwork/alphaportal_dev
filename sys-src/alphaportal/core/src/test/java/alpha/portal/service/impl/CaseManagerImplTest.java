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

import org.appfuse.model.User;
import org.appfuse.service.impl.BaseManagerMockTestCase;
import org.jmock.Expectations;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import alpha.portal.dao.AlphaCaseDao;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCase;

/**
 * The Class CaseManagerImplTest.
 */
public class CaseManagerImplTest extends BaseManagerMockTestCase {

	/** The manager. */
	private CaseManagerImpl manager = null;

	/** The dao. */
	private AlphaCaseDao dao = null;

	/**
	 * Sets the up.
	 */
	@Before
	public void setUp() {
		this.dao = this.context.mock(AlphaCaseDao.class);
		this.manager = new CaseManagerImpl(this.dao);
	}

	/**
	 * Tear down.
	 */
	@After
	public void tearDown() {
		this.manager = null;
	}

	/**
	 * Test get case.
	 */
	@Test
	public void testGetCase() {
		final String id = "7L";
		final AlphaCase Case = new AlphaCase();

		this.context.checking(new Expectations() {
			{
				this.one(CaseManagerImplTest.this.dao).get(
						this.with(Expectations.equal(id)));
				this.will(Expectations.returnValue(Case));
			}
		});

		AlphaCase result = this.manager.get(id);
		Assert.assertSame(Case, result);

		this.context.checking(new Expectations() {
			{
				this.one(CaseManagerImplTest.this.dao).get(
						this.with(Expectations.equal(id)));
				this.will(Expectations.returnValue(Case));
			}
		});

		result = this.manager.getCase(id);
		Assert.assertSame(Case, result);
	}

	/**
	 * Test get cases.
	 */
	@Test
	public void testGetCases() {
		final List<AlphaCase> Cases = new ArrayList<AlphaCase>();

		this.context.checking(new Expectations() {
			{
				this.one(CaseManagerImplTest.this.dao).getAll();
				this.will(Expectations.returnValue(Cases));
			}
		});

		List<AlphaCase> result = this.manager.getAll();
		Assert.assertSame(Cases, result);

		this.context.checking(new Expectations() {
			{
				this.one(CaseManagerImplTest.this.dao).getAll();
				this.will(Expectations.returnValue(Cases));
			}
		});

		result = this.manager.getCases();
		Assert.assertSame(Cases, result);
	}

	/**
	 * Test save case.
	 */
	@Test
	public void testSaveCase() {
		final AlphaCase Case = new AlphaCase();

		this.context.checking(new Expectations() {
			{
				this.one(CaseManagerImplTest.this.dao).save(
						this.with(Expectations.same(Case)));
				this.will(Expectations.returnValue(Case));
			}
		});

		AlphaCase case2 = this.manager.save(Case);
		Assert.assertEquals(Case, case2);

		this.context.checking(new Expectations() {
			{
				this.one(CaseManagerImplTest.this.dao).save(
						this.with(Expectations.same(Case)));
				this.will(Expectations.returnValue(Case));
			}
		});

		case2 = this.manager.saveCase(Case);
		Assert.assertEquals(Case, case2);
	}

	/**
	 * Test remove case.
	 */
	@Test
	public void testRemoveCase() {
		final String id = "-11L";

		this.context.checking(new Expectations() {
			{
				this.one(CaseManagerImplTest.this.dao).remove(
						this.with(Expectations.equal(id)));
			}
		});

		this.manager.remove(id);

		this.context.checking(new Expectations() {
			{
				this.one(CaseManagerImplTest.this.dao).remove(
						this.with(Expectations.equal(id)));
			}
		});

		this.manager.removeCase(id);
	}

	/**
	 * Test find by name.
	 */
	@Test
	public void testFindByName() {

		final String searchString = "Akte";

		this.context.checking(new Expectations() {
			{
				this.one(CaseManagerImplTest.this.dao).findByName(
						this.with(Expectations.equal(searchString)));
			}
		});

		this.manager.findByName(searchString);

	}

	/**
	 * Test find by participant.
	 */
	@Test
	public void testFindByParticipant() {
		final User user = new User();
		user.setId(-1L);

		this.context.checking(new Expectations() {
			{
				this.one(CaseManagerImplTest.this.dao).findByParticipant(
						this.with(Expectations.equal(user)));
			}
		});

		this.manager.findByParticipant(user);

	}

	/**
	 * Test update card order.
	 */
	@Test
	public void testUpdateCardOrder() {
		final AlphaCase aCase = new AlphaCase();
		aCase.setCaseId("123");
		final AlphaCard card1 = new AlphaCard(aCase);
		card1.getAlphaCardIdentifier().setCardId("1");
		final AlphaCard card2 = new AlphaCard(aCase);
		card2.getAlphaCardIdentifier().setCardId("2");
		final AlphaCard card3 = new AlphaCard(aCase);
		card3.getAlphaCardIdentifier().setCardId("3");
		aCase.addAlphaCard(card1);
		aCase.addAlphaCard(card2);
		aCase.addAlphaCard(card3);

		final List<String> order = new LinkedList<String>();
		order.add("2");
		order.add("3");
		order.add("1");

		this.context.checking(new Expectations() {
			{
				this.one(CaseManagerImplTest.this.dao).save(aCase);
			}
		});
		this.manager.updateCardOrder(aCase, order);

		Assert.assertEquals(card2, aCase.getAlphaCards().get(0));
		Assert.assertEquals(card3, aCase.getAlphaCards().get(1));
		Assert.assertEquals(card1, aCase.getAlphaCards().get(2));
	}

	/**
	 * Test remove.
	 */
	@Test
	public void testRemove() {
		final AlphaCase aCase = new AlphaCase();
		aCase.setCaseId("123");
		final AlphaCard card1 = new AlphaCard(aCase);
		card1.getAlphaCardIdentifier().setCardId("1");
		final AlphaCard card2 = new AlphaCard(aCase);
		card2.getAlphaCardIdentifier().setCardId("2");
		final AlphaCard card3 = new AlphaCard(aCase);
		card3.getAlphaCardIdentifier().setCardId("3");
		aCase.addAlphaCard(card1);
		aCase.addAlphaCard(card2);
		aCase.addAlphaCard(card3);

		this.context.checking(new Expectations() {
			{
				this.one(CaseManagerImplTest.this.dao).save(aCase);
			}
		});
		this.manager.removeAlphaCard(card1);

		Assert.assertEquals(2, aCase.getAlphaCards().size());
		Assert.assertEquals(card2, aCase.getAlphaCards().get(0));
		Assert.assertEquals(card3, aCase.getAlphaCards().get(1));

	}
}
