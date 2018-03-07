package opmodes;

import android.util.EventLog;

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.CRServo;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorSimple;
import com.qualcomm.robotcore.hardware.Gamepad;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;
import com.vuforia.HINT;

import javax.net.ssl.HostnameVerifier;

import examples.FourWheelDriveTaskExample;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.DeadmanMotorTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.PersistentTelemetryTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;
import team25core.TankDriveTask;

/**
 * Created by Lizzie on 11/4/2017.
 */

@TeleOp(name = "Disbelief Teleop")
public class LatteTeleop extends Robot {
    // Drivetrain and mechanism declarations.
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;

    private DcMotor relicExtender;
    private DcMotor relicCaster;
    private Servo relicGrabber;

    private DcMotor glyphElevator;
    private Servo glyphLGrabber;
    private Servo glyphRGrabber;
    private CRServo glyphSlider;
    private Servo jewelXServo;
    private Servo jewelYServo;

    private FourWheelDirectDrivetrain fwd;

    // Constants.
    public static final double GLYPH_OPEN_LEFT_POSITION = HisaishiCalibration.GLYPH_OPEN_LEFT_POSITION;
    public static final double GLYPH_CLOSE_LEFT_POSITION = HisaishiCalibration.GLYPH_CLOSE_LEFT_POSITION;
    public static final double GLYPH_OPEN_RIGHT_POSITION = HisaishiCalibration.GLYPH_OPEN_RIGHT_POSITION;
    public static final double GLYPH_CLOSE_RIGHT_POSITION = HisaishiCalibration.GLYPH_CLOSE_RIGHT_POSITION;
    public static final double GLYPH_HALF_OPEN_LEFT_POSITION = HisaishiCalibration.GLYPH_HALF_OPEN_LEFT_POSITION;
    public static final double GLYPH_HALF_OPEN_RIGHT_POSITION = HisaishiCalibration.GLYPH_HALF_OPEN_RIGHT_POSITION;
    public static final double GLYPH_QUARTER_OPEN_LEFT_POSITION = HisaishiCalibration.GLYPH_QUARTER_OPEN_LEFT_POSITION;
    public static final double GLYPH_QUARTER_OPEN_RIGHT_POSITION = HisaishiCalibration.GLYPH_QUARTER_OPEN_RIGHT_POSITION;

    public static final double RELIC_EXTENDER_POWER = HisaishiCalibration.RELIC_EXTENDER_POWER;
    public static final double RELIC_CASTER_POWER = HisaishiCalibration.RELIC_CASTER_POWER;
    public static final double RELIC_GRABBER_CLOSE = HisaishiCalibration.RELIC_GRABBER_CLOSE_POSITION;
    public static final double RELIC_GRABBER_OPEN = HisaishiCalibration.RELIC_GRABBER_OPEN_POSITION;

    private final double GLYPH_LIFT_ELEVATOR_POWER = HisaishiCalibration.GLYPH_LIFT_ELEVATOR_POWER;
    private final double GLYPH_DROP_ELEVATOR_POWER = HisaishiCalibration.GLYPH_DROP_ELEVATOR_POWER;
    public static final double GLYPH_LEFT_SLIDE_POWER = HisaishiCalibration.GLYPH_LEFT_SLIDE_POWER;
    public static final double GLYPH_RIGHT_SLIDE_POWER = HisaishiCalibration.GLYPH_RIGHT_SLIDE_POWER;
    public static final double GLYPH_STOP_SLIDE_POWER = HisaishiCalibration.GLYPH_STOP_SLIDE_POWER;

    private DeadReckonPath depositGlyphPath;
    private TankDriveTask tankDriveTask;

    private boolean isOpen = false;
    private boolean isDown = false;

    @Override
    public void init() {
        // Drivetrain.
        frontLeft = hardwareMap.dcMotor.get("frontL");
        frontRight = hardwareMap.dcMotor.get("frontR");
        backLeft = hardwareMap.dcMotor.get("backL");
        backRight = hardwareMap.dcMotor.get("backR");

        fwd = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);

        fwd.resetEncoders();
        fwd.encodersOn();
        fwd.setSplitPersonalityMotorDirection(true);


        // Relic.
        relicExtender = hardwareMap.dcMotor.get("relicExtender");
        relicCaster = hardwareMap.dcMotor.get("relicCaster");
        relicGrabber = hardwareMap.servo.get("relicGrabber");

        //Glyph.
        glyphElevator = hardwareMap.dcMotor.get("glyphElevator");
        glyphRGrabber = hardwareMap.servo.get("glyphRightGrabber");
        glyphLGrabber = hardwareMap.servo.get("glyphLeftGrabber");
        glyphSlider = hardwareMap.crservo.get("glyphSlider");

        // Jewel.
        jewelXServo = hardwareMap.servo.get("jewelXAxis");
        jewelYServo = hardwareMap.servo.get("jewelYAxis");

        // Initialization positions.
        jewelXServo.setPosition(HisaishiCalibration.JEWEL_X_AXIS_BACK);
        jewelYServo.setPosition(HisaishiCalibration.JEWEL_Y_AXIS_STOWED);

        depositGlyphPath = new DeadReckonPath();
    }

    @Override
    public void handleEvent(RobotEvent e) {
        // Continue on your scrolling adventure...
    }

    @Override
    public void start() {
        /* Driver One */
        tankDriveTask = new TankDriveTask(this, fwd);
        addTask(tankDriveTask);

        // Hug & Extend Relic.
        DeadmanMotorTask hugRelic = new DeadmanMotorTask(this, relicExtender, RELIC_EXTENDER_POWER, GamepadTask.GamepadNumber.GAMEPAD_1, DeadmanMotorTask.DeadmanButton.LEFT_BUMPER);
        DeadmanMotorTask extendRelic = new DeadmanMotorTask(this, relicExtender, -RELIC_EXTENDER_POWER, GamepadTask.GamepadNumber.GAMEPAD_1, DeadmanMotorTask.DeadmanButton.LEFT_TRIGGER);
        this.addTask(hugRelic);
        this.addTask(extendRelic);

        // Glyph.
        DeadmanMotorTask raiseGlyph = new DeadmanMotorTask(this, glyphElevator, GLYPH_LIFT_ELEVATOR_POWER, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.LEFT_BUMPER);
        DeadmanMotorTask dropGlyph = new DeadmanMotorTask(this, glyphElevator, -GLYPH_DROP_ELEVATOR_POWER, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.LEFT_TRIGGER);
        this.addTask(raiseGlyph);
        this.addTask(dropGlyph);

        // Relic.
        DeadmanMotorTask castRelic = new DeadmanMotorTask(this, relicCaster, -RELIC_CASTER_POWER, GamepadTask.GamepadNumber.GAMEPAD_1, DeadmanMotorTask.DeadmanButton.BUTTON_X);
        DeadmanMotorTask reelRelic = new DeadmanMotorTask(this, relicCaster, RELIC_CASTER_POWER, GamepadTask.GamepadNumber.GAMEPAD_1, DeadmanMotorTask.DeadmanButton.BUTTON_Y);
        this.addTask(castRelic);
        this.addTask(reelRelic);

        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                RobotLog.i("Gamepad Event", event.kind);
                if (event.kind == EventKind.BUTTON_A_DOWN) {
                    tankDriveTask.slowDown(true);
                } else if (event.kind == EventKind.BUTTON_B_DOWN) {
                    tankDriveTask.slowDown(false);
                } else if (event.kind == EventKind.DPAD_UP_DOWN) {
                    if (isDown) {
                        jewelYServo.setPosition(HisaishiCalibration.JEWEL_Y_AXIS_DEPLOYED);
                        isDown = false;
                    } else {
                        jewelYServo.setPosition(HisaishiCalibration.JEWEL_Y_AXIS_STOWED);
                        isDown = true;
                    }
                } else if (event.kind == EventKind.DPAD_DOWN_DOWN) {
                    jewelXServo.setPosition(HisaishiCalibration.JEWEL_X_AXIS_FORWARD);
                } else if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {
                    glyphDeposit(this.robot);
                    RobotLog.i("Glyph deposit method actived.");
                } else if (event.kind == EventKind.RIGHT_TRIGGER_DOWN) {
                    relicExtension();
                }
            }
        });

        /* Driver Two */
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                if (event.kind == EventKind.BUTTON_A_DOWN) {
                    if (isOpen) {
                        relicGrabber.setPosition(RELIC_GRABBER_CLOSE);
                        isOpen = false;
                    } else {
                        relicGrabber.setPosition(RELIC_GRABBER_OPEN);
                        isOpen = true;
                    }
                } else if (event.kind == EventKind.BUTTON_X_DOWN) {
                    servoPosQuarterOpen();
                } else if (event.kind == EventKind.DPAD_LEFT_DOWN) {
                    glyphSlider.setPower(GLYPH_LEFT_SLIDE_POWER);
                } else if (event.kind == EventKind.DPAD_RIGHT_DOWN) {
                    glyphSlider.setPower(GLYPH_RIGHT_SLIDE_POWER);
                } else if (event.kind == EventKind.DPAD_LEFT_UP|| event.kind == EventKind.DPAD_RIGHT_UP) {
                    glyphSlider.setPower(GLYPH_STOP_SLIDE_POWER);
                } else if (event.kind == EventKind.BUTTON_Y_DOWN) {
                    servoPosHalfOpen();
                } else if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {
                    servoPosOpen();
                } else if (event.kind == EventKind.RIGHT_TRIGGER_DOWN) {
                    servoPosClose();
                }
            }
        });
    }

    protected void servoPosOpen() {
        glyphLGrabber.setPosition(GLYPH_OPEN_LEFT_POSITION);
        glyphRGrabber.setPosition(GLYPH_OPEN_RIGHT_POSITION);
    }

    protected void servoPosQuarterOpen() {
        glyphLGrabber.setPosition(GLYPH_QUARTER_OPEN_LEFT_POSITION);
        glyphRGrabber.setPosition(GLYPH_QUARTER_OPEN_RIGHT_POSITION);
    }

    protected void servoPosHalfOpen() {
        glyphLGrabber.setPosition(GLYPH_HALF_OPEN_LEFT_POSITION);
        glyphRGrabber.setPosition(GLYPH_HALF_OPEN_RIGHT_POSITION);
    }

    protected void servoPosClose()
    {
        glyphLGrabber.setPosition(GLYPH_CLOSE_LEFT_POSITION);
        glyphRGrabber.setPosition(GLYPH_CLOSE_RIGHT_POSITION);
    }

    protected void depositPath(Robot robot)
    {
        depositGlyphPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, 0.2);
        depositGlyphPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, -0.5);
        depositGlyphPath.addSegment(DeadReckonPath.SegmentType.TURN, 140, 0.3);

        tankDriveTask.stop();
        robot.addTask(new DeadReckonTask(robot, depositGlyphPath, fwd) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                if(event.kind == EventKind.PATH_DONE) {
                    addTask(tankDriveTask);
                    // consider: when does "start" happen?
                }
            }
        });
    }

    protected void glyphDeposit(Robot robot) {
        servoPosQuarterOpen();
        robot.addTask(new SingleShotTimerTask(this, 500) {
            @Override
            public void handleEvent(RobotEvent e) {
                RobotLog.i("104 Initiating automated glyph placement.");
                depositPath(this.robot);
            }
        });
    }

    protected void relicExtension() {
        // ...more spicy code coming soon
    }

}


