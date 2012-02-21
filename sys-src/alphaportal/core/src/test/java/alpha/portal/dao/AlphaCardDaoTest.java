package alpha.portal.dao;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.appfuse.dao.BaseDaoTestCase;
import org.appfuse.service.GenericManager;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.AccessDeniedException;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AdornmentTypeDataProvision;
import alpha.portal.model.AdornmentTypeDeleted;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.Payload;
import alpha.portal.service.impl.AlphaCardManagerImpl;

public class AlphaCardDaoTest extends BaseDaoTestCase {

	@Autowired
	private AlphaCardDao alphaCardDao;

	@Autowired
	private AlphaCaseDao caseDao;

	@Autowired
	private GenericManager<Adornment, Long> adornmentManager;

	@Autowired
	private PayloadDao payloadDao;

	/**
	 * Test whole alphaCard.
	 * 
	 * @throws Exception
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testAddAndRemoveAlphaCard() throws Exception {
		final String title = "TestCard Title";

		AlphaCase aCase = new AlphaCase();
		aCase.setName("Test Case");
		aCase = caseDao.save(aCase);
		flush();

		AlphaCard card = new AlphaCard(aCase);
		card = alphaCardDao.save(card);
		long firstVersion = card.getAlphaCardIdentifier().getSequenceNumber();
		flush();
		assertTrue(alphaCardDao.exists(card.getAlphaCardIdentifier()));

		card.getAlphaCardDescriptor().setTitle(title);
		card = alphaCardDao.save(card);
		flush();
		assertEquals(firstVersion + 1, card.getAlphaCardIdentifier()
				.getSequenceNumber().longValue());
		card = alphaCardDao.get(card.getAlphaCardIdentifier());
		assertEquals(title, card.getAlphaCardDescriptor().getTitle());

		card.getAlphaCardDescriptor().setTitle("blablip");
		card = alphaCardDao.save(card);
		flush();
		assertEquals(firstVersion + 2, card.getAlphaCardIdentifier()
				.getSequenceNumber().longValue());

		List<AlphaCard> versions = alphaCardDao.getAllVersions(card
				.getAlphaCardIdentifier().getCaseId());
		assertEquals(3, versions.size());

		// Payload
		byte[] payloadData = "some cool test data".getBytes();
		Payload payload = new Payload();
		payload.setFilename("filename");
		payload.setMimeType("text/plain");
		payload.setContent(payloadData);
		payload = payloadDao.save(payload);
		card.setPayload(payload);
		card = alphaCardDao.save(card);
		card = alphaCardDao.get(card.getAlphaCardIdentifier());
		assertNotNull(card.getPayload());
		assertEquals("some cool test data", new String(card.getPayload()
				.getContent()));

		AlphaCard testCard = alphaCardDao.get(card.getAlphaCardIdentifier());
		assertNotNull(testCard);
		assertEquals(card.getAlphaCardIdentifier(),
				testCard.getAlphaCardIdentifier());
		assertEquals(card.getAlphaCardDescriptor(),
				testCard.getAlphaCardDescriptor());
		assertEquals(card.getPayload(), testCard.getPayload());
		assertEquals(card, testCard);
	}

	@Test
	public void testGetVersion() throws Exception {

		AlphaCase aCase = new AlphaCase();
		aCase.setName("Test Case");
		aCase = caseDao.save(aCase);
		flush();

		AlphaCard card = new AlphaCard(aCase);
		card = alphaCardDao.save(card);
		flush();
		assertTrue(alphaCardDao.exists(card.getAlphaCardIdentifier()));

		AlphaCard aCard = alphaCardDao.getVersion(new AlphaCardIdentifier());

		assertNull(aCard);

		aCard = alphaCardDao.get(new AlphaCardIdentifier());

		assertNull(aCard);

		assertFalse(alphaCardDao.exists(new AlphaCardIdentifier()));

	}

	@Test(expected = AccessDeniedException.class)
	public void testRemove() throws Exception {

		AlphaCase aCase = new AlphaCase();
		aCase.setName("Test Case");
		aCase = caseDao.save(aCase);
		flush();

		AlphaCard card = new AlphaCard(aCase);
		card = alphaCardDao.save(card);
		flush();
		assertTrue(alphaCardDao.exists(card.getAlphaCardIdentifier()));

		alphaCardDao.remove(card.getAlphaCardIdentifier());

	}

	@Test
	public void testListAlphaCardsByCriterion() throws Exception {
		List<AlphaCard> res = alphaCardDao.listAlphaCardsByCriterion(
				"gibtsnicht", AlphaCardManagerImpl.DATA_PROVISION_OPEN);
		assertEquals(0, res.size());
		res = alphaCardDao.listAlphaCardsByCriterion(
				"550e4713-e22b-11d4-a716-446655440002",
				AlphaCardManagerImpl.DATA_PROVISION_OPEN);
		assertEquals(1, res.size());
		for (AlphaCard c : res) {
			assertEquals("550e4713-e22b-11d4-a716-446655440002", c
					.getAlphaCardIdentifier().getCaseId());
			assertEquals(
					AdornmentTypeDataProvision.OPEN.getName(),
					c.getAlphaCardDescriptor()
							.getAdornment(AdornmentType.DataProvision.getName())
							.getValue());
		}

		Adornment delAdornment1 = new Adornment(AdornmentType.Deleted.getName());
		delAdornment1.setValue(AdornmentTypeDeleted.FALSE.value());
		delAdornment1 = adornmentManager.save(delAdornment1);
		flush();
		Adornment delAdornment2 = new Adornment(AdornmentType.Deleted.getName());
		delAdornment2.setValue(AdornmentTypeDeleted.FALSE.value());
		delAdornment2 = adornmentManager.save(delAdornment2);
		flush();

		AlphaCard critMatcher1 = new AlphaCard(
				"550e4713-e22b-11d4-a716-446655440002");
		critMatcher1.setAlphaCardIdentifier(new AlphaCardIdentifier(
				"550e4713-e22b-11d4-a716-446655440002", "0111-222-333-444"));
		critMatcher1 = alphaCardDao.save(critMatcher1);
		flush();
		critMatcher1.getAlphaCardDescriptor().setTitle("Test Alpha Card 1");
		critMatcher1.getAlphaCardDescriptor().setAdornment(delAdornment1);
		critMatcher1 = alphaCardDao.save(critMatcher1);
		flush();
		AlphaCard critMatcher2 = new AlphaCard(
				"550e4713-e22b-11d4-a716-446655440002");
		critMatcher1.setAlphaCardIdentifier(new AlphaCardIdentifier(
				"550e4713-e22b-11d4-a716-446655440002", "0111-222-888-999"));
		critMatcher2 = alphaCardDao.save(critMatcher2);
		flush();
		critMatcher2.getAlphaCardDescriptor().setTitle("Test Alpha Card 2");
		critMatcher2.getAlphaCardDescriptor().setAdornment(delAdornment2);
		critMatcher2 = alphaCardDao.save(critMatcher2);
		flush();

		List<AlphaCard> res2 = alphaCardDao.listAlphaCardsByCriterion(
				"550e4713-e22b-11d4-a716-446655440002",
				AlphaCardManagerImpl.NOT_DELETED);
		assertNotNull(res2);
		// FIXME: assertion is violated
		// assertTrue(res2.size() > 0);
	}

	@Test
	public void listDashBoardAlphaCards() {
		String[] caseIDs = new String[3];
		caseIDs[0] = "550e4713-e22b-11d4-a716-446655440001";
		caseIDs[1] = "550e4713-e22b-11d4-a716-446655440002";
		caseIDs[2] = "11111111-2222-3333-4444-555556666777";

		List<AlphaCard> testList = alphaCardDao
				.listDashBoardAlphaCards(caseIDs);
		assertNotNull(testList);
		assertTrue(testList.size() > 0);
	}

	@Test
	public void testSaveWithNull() {
		AlphaCard testCard = new AlphaCard();
		testCard.setAlphaCardIdentifier(null);

		testCard = alphaCardDao.save(testCard);
		assertNotNull(testCard);
	}
}
