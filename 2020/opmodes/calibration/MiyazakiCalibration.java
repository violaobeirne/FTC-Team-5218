package opmodes.calibration;

import team25core.ColorSensorTask;

/**
 * Created by Lizzie on 10/1/2019.
 */
public class MiyazakiCalibration {

    public static final double SPEED_LIMIT = 0.6;

    // foundation migration and stone arms
    public static final double STONE_LEFT_ARM_DOWN = 238/256.0;
    public static final double STONE_LEFT_ARM_STOW = 65/256.0;
    public static final double STONE_RIGHT_ARM_DOWN = 28/256.0;
    public static final double STONE_RIGHT_ARM_STOW = 191/256.0;
    public static final double STONE_ARM_STOW = 80.0/256.0;
    public static final double STONE_ARM_PUSH = 256.0/256.0;

    public static final double ARM_LEFT_STOW = 111.0/256.0;
    public static final double ARM_LEFT_DOWN = 0/256.0;

    public static final double ARM_RIGHT_STOW = 0.0/360.0;
    public static final double ARM_RIGHT_DOWN = 198.0/256.0;

    // active wheel intake
    public static final double INTAKE_LEFT_COLLECT = 1.0;
    public static final double INTAKE_RIGHT_COLLECT = -1.0;

    public static final double INTAKE_LEFT_DISPENSE = -1.0;
    public static final double INTAKE_RIGHT_DISPENSE = 1.0;

    // claw
    public static final double NEW_CLAW_OPEN = 169.0 / 256.0; //version 1: 50, version 2: 225, version 3: 95, version 4: 50, version 5: 5
    public static final double NEW_CLAW_CLOSE = 5.0 / 256.0; //version 1: 200, version 2: 168, version 3: 170, version 4: 150, version 5: 120
    public static final double OLD_CLAW_OPEN = 50.0/256.0; //version 1: 50, version 2: 150, version 3: 50
    public static final double OLD_CLAW_CLOSE = 120.0/256.0;//version 1: 150, version 2: 170, version 3: 150, version 4: 120, version 5: 100, version 6: 90

    // vertical lift
    public static final double VLIFT_UP = -1.0;
    public static final double VLIFT_DOWN = 1.0;
    public static final int LEVEL_ENCODER_VALUE = 2510;

    // horizontal lift
    public static final double HLIFT_OUT = -1;
    public static final double HLIFT_IN = 1;
    public static final double HLIFT_STOP = 0;
    public static final double HLIFT_DIST = 1.00;

    // encoders per inch
    public static final int ENCODERS_PER_CYCLE = 950;
    public static final int ENCODERS_PER_INCH = ENCODERS_PER_CYCLE / 4;

    // alliance multiplier (blue is positive, red is negative)
    public static final int ALLIANCE_MULTIPLIER = 1;


    /*

     REV CONTROL HUB 1 CONFIGURATION (BACK OF ROBOT)
        MOTORS (DRIVETRAIN)
            0 - "backRight"     - drivetrain back left
            1 - "backLeft"      - drivetrain back right
            2 - "vLift"         - vertical lift
            3 - "hLift"         - horizontal lift

        SERVOS
            0 - "claw"          - claw
            1 - "leftStoneArm"  - left autonomous arm
            2 - "rightStoneArm" - right autonomous arm
            3 -

        DIGITAL DEVICES
            0 - "touchLeft"    - touch sensor left
            1 - "toughRight"   - touch sensor right

        REV EXPANSION HUB 2 CONFIGURATION (FRONT OF ROBOT)
            MOTORS (MECHANISMS)
            0 - "frontRight"    - drivetrain front left
            1 - "frontLeft"     - drivetrain front right
            2 - "leftIntake"    - active wheel intake left
            3 - "rightIntake"   - active wheel intake right

            0 - "leftArm"       - foundation migration left arm
            1 - "rightArm"      - foundation migration right arm
            2 -
            3 -

        DIGITAL DEVICES
            0 - "leftSky"       - left skystone color sensor
            1 - "rightSky"      - right skystone color sensor
            2 -
     */
}
