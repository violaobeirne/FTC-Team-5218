package opmodes;

import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorSimple;
import com.qualcomm.robotcore.hardware.Gamepad;
import com.qualcomm.robotcore.hardware.Servo;

import examples.FourWheelDriveTaskExample;
import team25core.DeadmanMotorTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.PersistentTelemetryTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankDriveTask;

/**
 * Created by Lizzie on 11/4/2017.
 */

@TeleOp(name = "5218 Teleop")
@Disabled
public class ChopinTeleop extends Robot{

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;

    // private DcMotor relicExtender;
    // private DcMotor relicCaster;
    private Servo relicGrabber;

    private DcMotor glyphElevator;
    private Servo glyphLGrabber;
    private Servo glyphRGrabber;
    private Servo glyphSlider;

    private FourWheelDirectDrivetrain fwd;

    public static final double GLYPH_OPEN_LEFT_POSITION = HisaishiCalibration.GLYPH_OPEN_LEFT_POSITION;
    public static final double GLYPH_CLOSE_LEFT_POSITION = HisaishiCalibration.GLYPH_CLOSE_LEFT_POSITION;
    public static final double GLYPH_OPEN_RIGHT_POSITION = HisaishiCalibration.GLYPH_OPEN_RIGHT_POSITION;
    public static final double GLYPH_CLOSE_RIGHT_POSITION = HisaishiCalibration.GLYPH_CLOSE_RIGHT_POSITION;
    public static final double GLYPH_HALF_OPEN_LEFT_POSITION = HisaishiCalibration.GLYPH_HALF_OPEN_LEFT_POSITION;
    public static final double GLYPH_HALF_OPEN_RIGHT_POSITION = HisaishiCalibration.GLYPH_HALF_OPEN_RIGHT_POSITION;

    public static final double RELIC_EXTENDER_POWER = HisaishiCalibration.RELIC_EXTENDER_POWER;
    public static final double RELIC_CASTER_POWER = HisaishiCalibration.RELIC_CASTER_POWER;
    public static final double RELIC_GRABBER_CLOSE = HisaishiCalibration.RELIC_GRABBER_CLOSE_POSITION;
    public static final double RELIC_GRABBER_OPEN = HisaishiCalibration.RELIC_GRABBER_OPEN_POSITION;

    private final double GLYPH_LIFT_ELEVATOR_POWER = HisaishiCalibration.GLYPH_LIFT_ELEVATOR_POWER;
    private final double GLYPH_DROP_ELEVATOR_POWER = HisaishiCalibration.GLYPH_DROP_ELEVATOR_POWER;
    
    

    @Override
    public void init() {
        // Drivetrain.
        frontLeft = hardwareMap.dcMotor.get("frontL");
        frontRight = hardwareMap.dcMotor.get("frontR");
        backLeft = hardwareMap.dcMotor.get("backL");
        backRight = hardwareMap.dcMotor.get("backR");

        frontLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODERS);
        frontLeft.setDirection(DcMotorSimple.Direction.REVERSE);
        frontRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontRight.setMode(DcMotor.RunMode.RUN_USING_ENCODERS);
        frontRight.setDirection(DcMotorSimple.Direction.REVERSE);
        backLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODERS);
        backLeft.setDirection(DcMotorSimple.Direction.REVERSE);
        backRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backRight.setMode(DcMotor.RunMode.RUN_USING_ENCODERS);
        backRight.setDirection(DcMotorSimple.Direction.REVERSE );

        fwd = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);

        // Relic.
        // relicExtender = hardwareMap.dcMotor.get("relicExtender");
        // relicCaster = hardwareMap.dcMotor.get("relicCaster");
        // relicGrabber = hardwareMap.servo.get("relicGrabber");


        //Glyph.
        glyphElevator = hardwareMap.dcMotor.get("glyphElevator");
        glyphRGrabber = hardwareMap.servo.get("glyphRightGrabber");
        glyphLGrabber = hardwareMap.servo.get("glyphLeftGrabber");
        // glyphSlider = hardwareMap.dcMotor.get("glyphSlider");

    }

    @Override
    public void handleEvent(RobotEvent e) {
        // Keep scrolling...
    }

    @Override
    public void start() {
        /* Driver One */
        this.addTask(new TankDriveTask(this, fwd));

        // Hug & Extend Relic.
        // DeadmanMotorTask hugRelic = new DeadmanMotorTask(this, relicExtender, RELIC_EXTENDER_POWER, GamepadTask.GamepadNumber.GAMEPAD_1, DeadmanMotorTask.DeadmanButton.LEFT_BUMPER);
        // DeadmanMotorTask extendRelic = new DeadmanMotorTask(this, relicExtender, -RELIC_EXTENDER_POWER, GamepadTask.GamepadNumber.GAMEPAD_1, DeadmanMotorTask.DeadmanButton.LEFT_TRIGGER);
        // this.addTask(hugRelic);
        // this.addTask(extendRelic);

        // Glyph.
        DeadmanMotorTask raiseGlyph = new DeadmanMotorTask(this, glyphElevator, GLYPH_LIFT_ELEVATOR_POWER, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.LEFT_BUMPER);
        DeadmanMotorTask dropGlyph = new DeadmanMotorTask(this, glyphElevator, -GLYPH_DROP_ELEVATOR_POWER, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.LEFT_TRIGGER);
        this.addTask(raiseGlyph);
        this.addTask(dropGlyph);

        // Relic.
        // DeadmanMotorTask castRelic = new DeadmanMotorTask(this, relicCaster, RELIC_CASTER_POWER, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_X);
        // DeadmanMotorTask reelRelic = new DeadmanMotorTask(this, relicCaster, -RELIC_CASTER_POWER, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_Y);
        // this.addTask(castRelic);
        // this.addTask(reelRelic);

        /* Driver Two */
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                if (event.kind == EventKind.BUTTON_A_DOWN) {
                    // relicGrabber.setPosition(RELIC_GRABBER_OPEN);
                    // relicGrabber.setPosition(RELIC_GRABBER_CLOSE);
                    // TODO: Make this a toggle command.
                } else if (event.kind == EventKind.BUTTON_X_DOWN) {
                    glyphSlider.setPosition(-1.0);
                } else if (event.kind == EventKind.BUTTON_B_DOWN) {
                    glyphSlider.setPosition(1.0);
                } else if (event.kind == EventKind.BUTTON_X_UP || event.kind == EventKind.BUTTON_B_UP) {
                    glyphSlider.setPosition(0);
                } else if (event.kind == EventKind.BUTTON_Y_DOWN) {
                    glyphLGrabber.setPosition(GLYPH_HALF_OPEN_LEFT_POSITION);
                    glyphRGrabber.setPosition(GLYPH_HALF_OPEN_RIGHT_POSITION);
                } else if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {
                    glyphLGrabber.setPosition(GLYPH_OPEN_LEFT_POSITION);
                    glyphRGrabber.setPosition(GLYPH_OPEN_RIGHT_POSITION);
                } else if (event.kind == EventKind.RIGHT_TRIGGER_DOWN) {
                    glyphLGrabber.setPosition(GLYPH_CLOSE_LEFT_POSITION);
                    glyphRGrabber.setPosition(GLYPH_CLOSE_RIGHT_POSITION);
                }
            }
        });
    }
}
