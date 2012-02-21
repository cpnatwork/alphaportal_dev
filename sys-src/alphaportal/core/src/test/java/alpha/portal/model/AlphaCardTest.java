package alpha.portal.model;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class AlphaCardTest {

    @Test(expected = IllegalStateException.class)
    public void testAlphaCardAlphaCardDescriptorNullException() {
	AlphaCardDescriptor testDescriptor = new AlphaCardDescriptor();
	testDescriptor.setAlphaCardIdentifier(null);
	@SuppressWarnings("unused")
	AlphaCard testAlphaCard = new AlphaCard(testDescriptor);
    }

    @Test(expected = IllegalStateException.class)
    public void testAlphaCardAlphaCardDescriptorCardIdEmptyException() {
	AlphaCardDescriptor testDescriptor = new AlphaCardDescriptor();
	testDescriptor.setAlphaCardIdentifier(new AlphaCardIdentifier("test",
	""));
	@SuppressWarnings("unused")
	AlphaCard testAlphaCard = new AlphaCard(testDescriptor);
    }

    @Test(expected = IllegalStateException.class)
    public void testAlphaCardAlphaCardDescriptorCaseIdEmptyException() {
	AlphaCardDescriptor testDescriptor = new AlphaCardDescriptor();
	testDescriptor.setAlphaCardIdentifier(new AlphaCardIdentifier("",
	"test"));
	@SuppressWarnings("unused")
	AlphaCard testAlphaCard = new AlphaCard(testDescriptor);
    }

    @Test
    public void testAlphaCardAlphaCardDescriptor() {
	String testCaseId = "myTestCaseId";
	String testCardId = "myTestCardId";

	AlphaCardDescriptor testDescriptor = new AlphaCardDescriptor();
	testDescriptor.setAlphaCardIdentifier(new AlphaCardIdentifier(
		testCaseId, testCardId));

	AlphaCard testAlphaCard = new AlphaCard(testDescriptor);

	assertTrue(testAlphaCard.getAlphaCardDescriptor()
		.equals(testDescriptor));
    }

    @Test(expected = IllegalStateException.class)
    public void testAlphaCardAlphaCaseException() {
	AlphaCase testAlphaCase = new AlphaCase();
	@SuppressWarnings("unused")
	AlphaCard testAlphaCard = new AlphaCard(testAlphaCase);
    }

    @Test
    public void testSetAlphaCardIdentifier() {
	String testCaseId = "myTestCaseId";
	String testCardId = "myTestCardId";

	AlphaCardDescriptor testDescriptor = new AlphaCardDescriptor();
	AlphaCardIdentifier testAlphaCardIdentifier = new AlphaCardIdentifier(
		testCaseId, testCardId);
	testDescriptor.setAlphaCardIdentifier(testAlphaCardIdentifier);

	AlphaCard testAlphaCard = new AlphaCard();
	testAlphaCard.setAlphaCardIdentifier(testAlphaCardIdentifier);

	assertTrue(testAlphaCard.getAlphaCardIdentifier().equals(
		testAlphaCardIdentifier));
    }

    @Test
    public void testSetAlphaCardDescriptor() {
	String testCaseId = "myTestCaseId";
	String testCardId = "myTestCardId";

	AlphaCard testAlphaCard = new AlphaCard("myTestCaseId");

	AlphaCardDescriptor testDescriptor = new AlphaCardDescriptor();
	AlphaCardIdentifier testAlphaCardIdentifier = new AlphaCardIdentifier(
		testCaseId, testCardId);
	testDescriptor.setAlphaCardIdentifier(testAlphaCardIdentifier);

	testAlphaCard.setAlphaCardIdentifier(null);
	testAlphaCard.setAlphaCardDescriptor(testDescriptor);

	assertTrue(testAlphaCard.getAlphaCardIdentifier().equals(
		testDescriptor.getAlphaCardIdentifier()));
    }

    @Test
    public void testSetAlphaCardDescriptorSecondCondition() {
	String testCaseId = "myTestCaseId";
	String testCardId = "myTestCardId";

	AlphaCard testAlphaCard = new AlphaCard(testCaseId);

	AlphaCardDescriptor testDescriptor = new AlphaCardDescriptor();
	AlphaCardIdentifier testAlphaCardIdentifier = new AlphaCardIdentifier(
		testCaseId, testCardId);
	testDescriptor.setAlphaCardIdentifier(testAlphaCardIdentifier);

	testAlphaCard.setAlphaCardIdentifier(null);
	testAlphaCard.getAlphaCardDescriptor().setAlphaCardIdentifier(null);
	testAlphaCard.setAlphaCardDescriptor(testDescriptor);

	assertTrue(testAlphaCard.getAlphaCardIdentifier().equals(
		testDescriptor.getAlphaCardIdentifier()));

	assertFalse(!testAlphaCard.getAlphaCardIdentifier().equals(
		testDescriptor.getAlphaCardIdentifier()));
    }

    @Test
    public void testEqualsObject() {
	AlphaCard testAlphaCard = new AlphaCard();
	assertFalse(testAlphaCard.equals(new AlphaCase()));
    }

    @Test
    public void testToString() {
	AlphaCard testAlphaCard = new AlphaCard();
	testAlphaCard.setAlphaCardDescriptor(new AlphaCardDescriptor());
	testAlphaCard.setAlphaCardIdentifier(new AlphaCardIdentifier());
	testAlphaCard.setAlphaCase(new AlphaCase());
	testAlphaCard.setPayload(new Payload());

	assertTrue(testAlphaCard.toString().length() > 0);
    }

    @Test
    public void testGetAlphaCase() {
	AlphaCard testAlphaCard = new AlphaCard();
	AlphaCase testAlphaCase = new AlphaCase();

	testAlphaCard.setAlphaCase(testAlphaCase);

	assertTrue(testAlphaCard.getAlphaCase().equals(testAlphaCase));

	AlphaCard testAlphaCard2 = new AlphaCard();
	assertFalse(testAlphaCard2.getAlphaCase() != null);
    }

    @Test
    public void testGetAlphaCardDescriptor() {
	AlphaCard testAlphaCard = new AlphaCard();
	AlphaCardDescriptor testAlphaCardDescriptor = new AlphaCardDescriptor();

	testAlphaCard.setAlphaCardDescriptor(testAlphaCardDescriptor);

	assertTrue(testAlphaCard.getAlphaCardDescriptor().equals(
		testAlphaCardDescriptor));
    }

}
