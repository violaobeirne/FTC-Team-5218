package opmodes;

/**
 * Created by Lizzie on 10/28/2017.
 */

public class HisaishiCalibration {
    public static final int TICKS_PER_DERGREE = 17;
    public static final int TICKS_PER_INCH = 79;
    public static final double TURN_SPEED = 0.2;
    public static final double MOVE_SPEED = 0.75;

    // glyph mechanisms
    public static final double GLYPH_LIFT_ELEVATOR_POWER = 0.6;
    public static final double GLYPH_DROP_ELEVATOR_POWER = 0.1;
    public static final double GLYPH_OPEN_LEFT_POSITION = 0.3;
    public static final double GLYPH_OPEN_RIGHT_POSITION = 0.7;
    public static final double GLYPH_CLOSE_LEFT_POSITION = 0;
    public static final double GLYPH_CLOSE_RIGHT_POSITION = 1.0;
    public static final double GLYPH_STOW_LEFT_POSITION = 1.0;
    public static final double GLYPH_STOW_RIGHT_POSITION = 0;
    public static final double GLYPH_HALF_OPEN_LEFT_POSITION = 0.25;
    public static final double GLYPH_HALF_OPEN_RIGHT_POSITION = 0.75;
    public static final double GLYPH_LEFT_SLIDE_POWER = 0;
    public static final double GLYPH_RIGHT_SLIDE_POWER = 255;
    public static final double GLYPH_STOP_SLIDE_POWER = 128;

    // relic mechanisms
    public static final double RELIC_GRABBER_OPEN_POSITION = 0.6;
    public static final double RELIC_GRABBER_CLOSE_POSITION = 0.3;
    public static final double RELIC_EXTENDER_POWER = 0.5;
    public static final double RELIC_CASTER_POWER = 0.55;
}
