package opmodes;
/**
 * Created by Lizzie on 10/1/2019.
 */
public class HisaishiCalibration {

    /* PROTOTYPES */
    // foundation migration

    public static final double ARM_LEFT_DOWN = 151.0/256.0;
    public static final double ARM_LEFT_STOW = 95.0/256.0;

    public static final double ARM_RIGHT_DOWN = 8.0/360.0;
    public static final double ARM_RIGHT_STOW = 57.0/256.0;

    // lazy susan
    public static final double SUSAN_STOW = 60.0/256.0;
    public static final double SUSAN_OUT = 80.0/256.0;

    // active wheel intake
    public static final double INTAKE_LEFT_COLLECT = 0.5;
    public static final double INTAKE_RIGHT_COLLECT = -0.5;

    public static final double INTAKE_LEFT_DISPENSE = -0.5;
    public static final double INTAKE_RIGHT_DISPENSE = 0.5;

    // two bar linkage
    public static final double TBAR_ARM_UP = 0.3;
    public static final double TBAR_ARM_DOWN = -0.3;

    // claw
    public static final double NEW_CLAW_OPEN = 225.0/256.0; //version 1: 50, version 2: 225
    public static final double NEW_CLAW_CLOSE = 160.0 / 256.0; //version 1: 200, version 2: 168
    public static final double OLD_CLAW_OPEN = 50.0/256.0;
    public static final double OLD_CLAW_CLOSE = 150.0/256.0;

    // lift
    public static final double LIFT_UP = -0.5;
    public static final double LIFT_DOWN = 0.5;


    /*
    REV EXPANSION HUB 4 CONFIGURATION (FRONT OF ROBOT)
        MOTORS (MECHANISMS)
            0 - "frontRight"     - drivetrain front left
            1 - "frontLeft"    - drivetrain front right
            2 - "leftIntake"    - active wheel intake left
            3 - "rightIntake"   - active wheel intake right

            0 - "leftArm"       - foundation migration left arm
            1 - "rightArm"      - foundation migration right arm
            2 -
            3 -

     REV CONTROL HUB 1 CONFIGURATION (BACK OF ROBOT)
        MOTORS (DRIVETRAIN)
            0 - "backRight"     - drivetrain back left
            1 - "backLeft"    - drivetrain back right
            2 - "lift"         - cascading lift
            3 -

        SERVOS
            0 - "susan"         - lazy susan
            1 - "claw"          - claw
            2 -
            3 -
        SERVOS
     */
}
