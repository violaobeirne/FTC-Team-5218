package opmodes;
/**
 * Created by Lizzie on 10/1/2019.
 */
public class HisaishiCalibration {

    /* PROTOTYPES */
    // foundation migration
    public static final double ARM_LEFT_DOWN = 143/360; //before 0
    public static final double ARM_LEFT_STOW = 91/360; //before 360

    public static final double ARM_RIGHT_DOWN = 0/360;
    public static final double ARM_RIGHT_STOW = 54/360; //before 360

    // lazy susan
    public static final double SUSAN_LEFT = -1;
    public static final double SUSAN_RIGHT = 1;

    // active wheel intake
    public static final double INTAKE_LEFT_COLLECT = 0.5;
    public static final double INTAKE_RIGHT_COLLECT = -0.5;

    public static final double INTAKE_LEFT_DISPENSE = -0.5;
    public static final double INTAKE_RIGHT_DISPENSE = 0.5;

    // two bar linkage
    public static final double TBAR_ARM_UP = 0.3;
    public static final double TBAR_ARM_DOWN = -0.3;

    // claw
    public static final double CLAW_OPEN = 225.0/256.0; //version 1: 50, version 2: 225
    public static final double CLAW_CLOSE = 168.0 / 256.0; //version 1: 200, version 2: 168

    // lift
    public static final double LIFT_UP = 0.5;
    public static final double LIFT_DOWN = -0.5;


    /*
    REV EXPANSION HUB 4 CONFIGURATION (FRONT OF ROBOT)
        MOTORS (MECHANISMS)
            0 - "frontLeft"     - drivetrain front left
            1 - "frontRight"    - drivetrain front right
            2 - "leftIntake"    - active wheel intake left
            3 - "rightIntake"   - active wheel intake right

        SERVOS
            0 -
            1 -
            2 -
            3 -

     REV CONTROL HUB 1 CONFIGURATION (BACK OF ROBOT)
        MOTORS (DRIVETRAIN)
            0 - "backLeft"     - drivetrain back left
            1 - "backRight"    - drivetrain back right
            2 - "lift"         - cascading lift
            3 - ""

        SERVOS
            0 - "susan"         - lazy susan
            1 - "leftArm"       - foundation migration left arm
            2 - "rightArm"      - foundation migration right arm
            3 - "claw"          - claw
     */
}
