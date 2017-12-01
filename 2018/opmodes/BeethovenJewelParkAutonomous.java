package opmodes;

import android.util.EventLog;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Gamepad;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.internal.android.dex.Leb128;

import team25core.ColorThiefTask;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.DeadmanMotorTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.PersistentTelemetryTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 11/11/2017.
 */

@Autonomous (name = "5218 Jewel + Park Autonomous")
public class BeethovenJewelParkAutonomous extends Robot {
    protected enum Alliance {
        BLUE,
        RED,
        DEFAULT,
    }

    protected enum StartingPosition {
        R1,
        R2,
        B1,
        B2,
        DEFAULT,
    }

    protected Alliance alliance;
    protected StartingPosition startingPosition;

    private static int TURN_MULTIPLIER = 0;
    private static int MOVE_MULTIPLIER = 0;

    private final double TURN_SPEED = HisaishiCalibration.TURN_SPEED;
    private final double MOVE_SPEED = HisaishiCalibration.MOVE_SPEED;

    public static final double GLYPH_OPEN_LEFT_POSITION = HisaishiCalibration.GLYPH_OPEN_LEFT_POSITION;
    public static final double GLYPH_CLOSE_LEFT_POSITION = HisaishiCalibration.GLYPH_CLOSE_LEFT_POSITION;
    public static final double GLYPH_OPEN_RIGHT_POSITION = HisaishiCalibration.GLYPH_OPEN_RIGHT_POSITION;
    public static final double GLYPH_CLOSE_RIGHT_POSITION = HisaishiCalibration.GLYPH_CLOSE_RIGHT_POSITION;

    public static final double JEWEL_ARM_DEPLOY = HisaishiCalibration.JEWEL_ARM_DEPLOY;
    public static final double JEWEL_ARM_STOW = HisaishiCalibration.JEWEL_ARM_STOW;
    public static final double JEWEL_ARM_FORWARD = HisaishiCalibration.JEWEL_ARM_FORWARD;
    public static final double JEWEL_ARM_BACK = HisaishiCalibration.JEWEL_ARM_BACK;
    public static final double JEWEL_ARM_NEUTRAL = HisaishiCalibration.JEWEL_ARM_NEUTRAL;

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private Servo glyphLGrabber;
    private Servo glyphRGrabber;
    private Servo jewelXServo;
    private Servo jewelYServo;

    private PersistentTelemetryTask persistentTelemetryTask;
    private GamepadTask gamepad;

    private DeadReckonPath pushJewel;
    private DeadReckonPath moveToSimplePark;
    private DeadReckonPath moveToPark;

    private FourWheelDirectDrivetrain drivetrain;
    private ColorThiefTask colorThiefTask;

    @Override
    public void handleEvent(RobotEvent e) {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            handleGamepadSelection(event);
        }
    }

    @Override
    public void init() {
        // assign gamepad variables to defaults
        alliance = Alliance.DEFAULT;
        startingPosition = StartingPosition.DEFAULT;

        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        persistentTelemetryTask = new PersistentTelemetryTask(this);
        addTask(persistentTelemetryTask);

        // persistentTelemetryTask.addData("ALLIANCE: ", "NOT SELECTED");
        // persistentTelemetryTask.addData("STARTING POSITION: ", "NOT SELECTED");

        frontLeft = hardwareMap.dcMotor.get("frontL");
        frontRight = hardwareMap.dcMotor.get("frontR");
        backLeft = hardwareMap.dcMotor.get("backL");
        backRight = hardwareMap.dcMotor.get("backR");

        frontLeft.setMode(DcMotor.RunMode.RESET_ENCODERS);
        frontLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODERS);
        frontRight.setMode(DcMotor.RunMode.RESET_ENCODERS);
        frontRight.setMode(DcMotor.RunMode.RUN_USING_ENCODERS);
        backLeft.setMode(DcMotor.RunMode.RESET_ENCODERS);
        backLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODERS);
        backRight.setMode(DcMotor.RunMode.RESET_ENCODERS);
        backRight.setMode(DcMotor.RunMode.RUN_USING_ENCODERS);

        glyphRGrabber = hardwareMap.servo.get("glyphRightGrabber");
        glyphLGrabber = hardwareMap.servo.get("glyphLeftGrabber");

        jewelXServo = hardwareMap.servo.get("jewelXAxis");
        jewelYServo = hardwareMap.servo.get("jewelYAxis");

        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);

        pushJewel = new DeadReckonPath();
        moveToSimplePark = new DeadReckonPath();
        moveToPark = new DeadReckonPath();

        detectParticle();
    }

    @Override
    public void start() {
        if (alliance == Alliance.RED) {
            if (startingPosition == StartingPosition.R1) {
                red1Init();
                glyphLGrabber.setPosition(GLYPH_CLOSE_LEFT_POSITION);
                glyphRGrabber.setPosition(GLYPH_CLOSE_RIGHT_POSITION);
                jewelYServo.setPosition(JEWEL_ARM_DEPLOY);
                addTask(new SingleShotTimerTask(this, 1000) {
                    @Override
                    public void handleEvent(RobotEvent e)
                    {
                        RobotLog.i("104 Glyph retrieved, preparing to push particle.");
                        pushParticle();
                    }
                });
            } else if (startingPosition == StartingPosition.R2) {
                red2Init();
                glyphLGrabber.setPosition(GLYPH_CLOSE_LEFT_POSITION);
                glyphRGrabber.setPosition(GLYPH_CLOSE_RIGHT_POSITION);
                jewelYServo.setPosition(JEWEL_ARM_DEPLOY);
                addTask(new SingleShotTimerTask(this, 1000) {
                    @Override
                    public void handleEvent(RobotEvent e)
                    {
                        RobotLog.i("104 Glyph retrieved, preparing to push particle.");
                        pushParticle();
                    }
                });
            }
        } else if (alliance == Alliance.BLUE) {
            if (startingPosition == StartingPosition.B1) {
                blue1Init();
                glyphLGrabber.setPosition(GLYPH_CLOSE_LEFT_POSITION);
                glyphRGrabber.setPosition(GLYPH_CLOSE_RIGHT_POSITION);
                jewelYServo.setPosition(JEWEL_ARM_DEPLOY);
                addTask(new SingleShotTimerTask(this, 1000) {
                    @Override
                    public void handleEvent(RobotEvent e)
                    {
                        RobotLog.i("104 Glyph retrieved, preparing to push particle.");
                        pushParticle();
                    }
                });
            } else if (startingPosition == StartingPosition.B2) {
                blue2Init();
                glyphLGrabber.setPosition(GLYPH_CLOSE_LEFT_POSITION);
                glyphRGrabber.setPosition(GLYPH_CLOSE_RIGHT_POSITION);
                jewelYServo.setPosition(JEWEL_ARM_DEPLOY);
                addTask(new SingleShotTimerTask(this, 1000) {
                    @Override
                    public void handleEvent(RobotEvent e)
                    {
                        RobotLog.i("104 Glyph retrieved, preparing to push particle.");
                        pushParticle();
                    }
                });
            }
        }
        moveToSimplePark.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 42, MOVE_MULTIPLIER * MOVE_SPEED);
        moveToPark.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 42, MOVE_MULTIPLIER * MOVE_SPEED);
    }

    public void handleGamepadSelection(GamepadTask.GamepadEvent event) {
        switch (event.kind) {
            case BUTTON_X_DOWN:
                alliance = Alliance.BLUE;
                persistentTelemetryTask.addData("ALLIANCE: ", "" + alliance);
                break;
            case BUTTON_B_DOWN:
                alliance = Alliance.RED;
                persistentTelemetryTask.addData("ALLIANCE: ", "" + alliance);
                break;
            case BUTTON_A_DOWN:
                colorThiefTask.setPollingMode(ColorThiefTask.PollingMode.ON);
                break;
            case BUTTON_Y_DOWN:
                colorThiefTask.setPollingMode(ColorThiefTask.PollingMode.OFF);
                break;
            case LEFT_BUMPER_DOWN:
                startingPosition = StartingPosition.R1;
                persistentTelemetryTask.addData("STARTING POSITION: ", "" + startingPosition);
                break;
            case LEFT_TRIGGER_DOWN:
                startingPosition = StartingPosition.R2;
                persistentTelemetryTask.addData("STARTING POSITION: ", "" + startingPosition);
                break;
            case RIGHT_BUMPER_DOWN:
                startingPosition = StartingPosition.B1;
                persistentTelemetryTask.addData("STARTING POSITION: ", "" + startingPosition);
                break;
            case RIGHT_TRIGGER_DOWN:
                startingPosition = startingPosition.B2;
                persistentTelemetryTask.addData("STARTING POSITION: ", "" + startingPosition);
                break;
        }
    }

    protected void red1Init() {
        MOVE_MULTIPLIER = -1;
        TURN_MULTIPLIER = 1;
    }

    protected void red2Init() {
        MOVE_MULTIPLIER = -1;
        TURN_MULTIPLIER = 1;
    }

    protected void blue1Init() {
        MOVE_MULTIPLIER = 1;
        TURN_MULTIPLIER = -1;
    }

    protected void blue2Init() {
        MOVE_MULTIPLIER = 1;
        TURN_MULTIPLIER = -1;
    }

    protected void detectParticle() {
        colorThiefTask = new ColorThiefTask(this) {
            @Override
            public void handleEvent(RobotEvent e) {
                ColorThiefTask.ColorThiefEvent event = (ColorThiefEvent) e;
                // TODO: add telemetry
                if (alliance == Alliance.RED) {
                    switch (event.kind) {
                        case RED:
                            persistentTelemetryTask.addData("DETECTED COLOR", "RED");
                            RobotLog.i("104 Detected red");
                            jewelXServo.setPosition(JEWEL_ARM_FORWARD);
                            break;
                        case BLUE:
                            persistentTelemetryTask.addData("DETECTED COLOR", "BLUE");
                            RobotLog.i("104 Detected blue");
                            jewelXServo.setPosition(JEWEL_ARM_BACK);
                            break;
                        case BLACK:
                            persistentTelemetryTask.addData("DETECTED COLOR", "BLACK");
                            RobotLog.i("104 Detected black");
                            break;
                    }
                } else if (alliance == Alliance.BLUE) {
                    switch(event.kind) {
                        case RED:
                            persistentTelemetryTask.addData("DETECTED COLOR", "RED");
                            RobotLog.i("104 Detected red");
                            pushJewel.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, -MOVE_MULTIPLIER * MOVE_SPEED);
                            break;
                        case BLUE:
                            persistentTelemetryTask.addData("DETECTED COLOR", "BLUE");
                            RobotLog.i("104 Detected blue");
                            pushJewel.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, -MOVE_MULTIPLIER * MOVE_SPEED);
                            break;
                        case BLACK:
                            persistentTelemetryTask.addData("DETECTED COLOR", "BLACK");
                            RobotLog.i("104 Detected black");
                            break;
                    }
                }
            }
        };
        this.addTask(colorThiefTask);
    }

    protected void pushParticle()
    {
        this.addTask(new DeadReckonTask(this, pushJewel, drivetrain) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch(event.kind) {
                    case PATH_DONE:
                        if (startingPosition == StartingPosition.B1 || startingPosition == StartingPosition.R1) {
                            RobotLog.i("104 Starting initial move.");
                            initialMove(moveToSimplePark);
                        } else if (startingPosition == StartingPosition.B2 || startingPosition == StartingPosition.B2) {
                            RobotLog.i("104 Starting initial move");
                            initialMove(moveToPark);
                        }
                        break;
                }
            }
        });
    }

    protected void initialMove(final DeadReckonPath path) {
        RobotLog.i("104 Finished knocking jewel.");
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        if (startingPosition == StartingPosition.R1 || startingPosition == StartingPosition.B1) {
                            RobotLog.i("104 closing glyph arms.");
                            glyphLGrabber.setPosition(GLYPH_CLOSE_LEFT_POSITION);
                            glyphRGrabber.setPosition(GLYPH_CLOSE_RIGHT_POSITION);
                            jewelXServo.setPosition(JEWEL_ARM_NEUTRAL);
                            jewelYServo.setPosition(JEWEL_ARM_STOW);
                        } else if (startingPosition == StartingPosition.R2 || startingPosition == StartingPosition.B2){
                            RobotLog.i("104 closing glyph arms.");
                            glyphLGrabber.setPosition(GLYPH_CLOSE_LEFT_POSITION);
                            glyphRGrabber.setPosition(GLYPH_CLOSE_RIGHT_POSITION);
                            jewelXServo.setPosition(JEWEL_ARM_NEUTRAL);
                            jewelYServo.setPosition(JEWEL_ARM_STOW);
                        }
                }
            }
        });
    }
}
