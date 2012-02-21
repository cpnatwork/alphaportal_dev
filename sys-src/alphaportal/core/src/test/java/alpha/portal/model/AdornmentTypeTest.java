package alpha.portal.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.apache.commons.lang.ArrayUtils;
import org.junit.Before;
import org.junit.Test;

public class AdornmentTypeTest {
    private AdornmentType type;

    private String name;
    private AdornmentValueType valueType;
    private AdornmentTypeRange range;
    private String defaultValue;

    @Before
    public void setUp() {
        defaultValue = AdornmentTypeVisibility.PRIVATE.value();
        valueType = AdornmentValueType.Enum;
        range = new AdornmentTypeRange(new String[] {
                AdornmentTypeVisibility.PRIVATE.value(),
                AdornmentTypeVisibility.PUBLIC.value() });

        type = AdornmentType.Visibility;
        type.setValueType(valueType);
        type.setValueRange(range);

        name = type.getName();
    }

    @Test
    public void testFromNameException() {
        assertNull(AdornmentType.fromName("nosuchvalue_$%&/()"));
    }

    @Test
    public void testFromName() {
        assertTrue(AdornmentType.fromName(AdornmentType.Contributor.getName()) == AdornmentType.Contributor);
    }

    @Test
    public void testValueType() {
        assertTrue(valueType == type.getValueType());

        type.setValueType(AdornmentValueType.String);
        assertFalse(valueType == type.getValueType());
    }

    @Test
    public void testValueRange() {
        assertTrue(range == type.getValueRange());

        type.setValueRange(new AdornmentTypeRange(
                new String[] { "falsches Array" }));
        assertFalse(range == type.getValueRange());
    }

    @Test
    public void testValueDefault() {
        assertTrue(defaultValue == type.getValueDefault());

        type.setValueDefault(AdornmentTypeVisibility.PUBLIC.value());
        assertFalse(defaultValue == type.getValueDefault());
    }

    @Test
    public void testValidate() {
        assertTrue(AdornmentType.Title.validate("bla"));
        assertFalse(AdornmentType.Visibility.validate("neeeein"));
        assertTrue(AdornmentType.Visibility
                .validate(AdornmentTypeVisibility.PRIVATE.value()));

        assertFalse(AdornmentType.PayloadVersionNumber.validate("abc"));
    }

    @Test
    public void testValidStrings() {
        String[] t = AdornmentType.Visibility.getValueRange().getValidStrings();
        assertEquals(2, t.length);
        assertTrue(ArrayUtils.contains(t, AdornmentTypeVisibility.PRIVATE
                .value()));
        assertTrue(ArrayUtils.contains(t, AdornmentTypeVisibility.PUBLIC
                .value()));
    }
}
