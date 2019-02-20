package opmodes;

/**
 * Created by Lizzie on 10/20/2018.
 */
public class VivaldiCalibration {
    public static final double BUNGEE_BOX_DEPLOY = 0.6;
    public static final double BUNGEE_BOX_STOW = -0.6;

    public static final double MARKER_DEPLOYED = 90.0 / 256.0;
    public static final double MARKER_STOWED = 251.0/256.0;
    public static final int LIFT_ENCODER_COUNT = 11981;
    public static final int BUNGEE_ENCODER_COUNT = 5;
    public static final double LIFT_LEFT_UP = -0.8;
    public static final double LIFT_LEFT_DOWN = 0.8;
    public static final double LIFT_RIGHT_UP = -0.8;
    public static final double LIFT_RIGHT_DOWN = 0.8;
    public static final double LIFT_STOP = 0.0;
    public static final double FOUR_BAR_UP = -0.6;
    public static final double FOUR_BAR_DOWN = 0.6;
}
