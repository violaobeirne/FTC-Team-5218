package opmodes;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 10/1/2019.
 */
public class HisaishiCalibration {

    /* PROTOTYPES */
    // foundation migration
    public static final double ARM_LEFT_DOWN = 0/360;
    public static final double ARM_LEFT_STOW = 360/360;

    public static final double ARM_RIGHT_DOWN = 0/360;
    public static final double ARM_RIGHT_STOW = 360/360;

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
    public static final double CLAW_OPEN = 50.0/256.0;
    public static final double CLAW_CLOSE = 200.0 / 256.0;

    // lift
    public static final double LIFT_UP = 0.5;
    public static final double LIFT_DOWN = -0.5;


    /* REV HUB 1 CONFIGURATION
        MOTORS (PROTOTYPE)
            0 - "barArm"        - two bar arm
            1 - "leftIntake"    - wheel intake left
            2 - "rightIntake"   - wheel intake right
            3 - "lift"          - cascading lift

        MOTORS (SOMEDAY)
            0 - "frontLeft"     - drivetrain front left
            1 - "frontRIght"    - drivetrain front right
            2 - "backLeft"      - drivetrain back left
            3. - "backRight"    - drivetrain back right

        SERVOS
            0 - "susan"         - lazy susan
            1 - "leftArm"       = foundation migration left arm
            2 - "rightArm"      - foundation migration right arm
            3 - "claw"          - claw
            4 -
     */
}
