package opmodes;

import android.util.EventLog;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Gamepad;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.internal.android.dex.Leb128;

import team25core.ColorThiefTask;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.DeadmanMotorTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 11/11/2017.
 */

@Autonomous (name = "5218 Jewel + Park Autonomous")
public class BeethovenJewelParkAutonomous extends Robot {
    protected enum AllianceColor {
        BLUE,
        RED,
        DEFAULT,
    }

    protected enum StartPosition {
        R1,
        R2,
        B1,
        B2,
        DEFAULT,
    }

    protected AllianceColor allianceColor;
    protected StartPosition startPosition;

    private static int TURN_MULTIPLIER = 0;
    private static int MOVE_MULTIPLIER = 0;

    private static boolean detectedRed = false;

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

    private GamepadTask gamepad;

    private DeadReckonPath pushJewel;
    private DeadReckonPath moveToSimplePark;
    private DeadReckonPath moveToPark;

    private FourWheelDirectDrivetrain drivetrain;
    private ColorThiefTask colorThiefTask;

    private Telemetry.Item particleColor;
    private Telemetry.Item polling;
    private Telemetry.Item alliance;
    private Telemetry.Item startingPosition;

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
        allianceColor = AllianceColor.DEFAULT;
        startPosition = StartPosition.DEFAULT;

        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        alliance.addData("Alliance: ", "NOT SELECTED");
        startingPosition.addData("Starting Position: ", "NOT SELECTED");
        particleColor.addData("Jewel Color:", "BLACK");
        polling.addData("Polling:", "OFF");

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
        if (allianceColor == AllianceColor.RED) {
            if (startPosition == StartPosition.R1) {
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
            } else if (startPosition == StartPosition.R2) {
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
        } else if (allianceColor == AllianceColor.BLUE) {
            if (startPosition == StartPosition.B1) {
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
            } else if (startPosition == StartPosition.B2) {
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
        moveToSimplePark.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, MOVE_MULTIPLIER * MOVE_SPEED);
        moveToPark.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, MOVE_MULTIPLIER * MOVE_SPEED);
    }

    public void handleGamepadSelection(GamepadTask.GamepadEvent event) {
        switch (event.kind) {
            case BUTTON_X_DOWN:
                allianceColor = AllianceColor.BLUE;
                alliance.setValue("BLUE");
                break;
            case BUTTON_B_DOWN:
                allianceColor = AllianceColor.RED;
                alliance.setValue("BLUE");
                break;
            case BUTTON_A_DOWN:
                colorThiefTask.setPollingMode(ColorThiefTask.PollingMode.ON);
                polling.setValue("ON");
                break;
            case BUTTON_Y_DOWN:
                polling.setValue("OFF");
                colorThiefTask.setPollingMode(ColorThiefTask.PollingMode.OFF);
                break;
            case LEFT_BUMPER_DOWN:
                startPosition = StartPosition.R1;
                startingPosition.setValue("R1");
                break;
            case LEFT_TRIGGER_DOWN:
                startPosition = StartPosition.R2;
                startingPosition.setValue("R2");
                break;
            case RIGHT_BUMPER_DOWN:
                startPosition = StartPosition.B1;
                startingPosition.setValue("B1");
                break;
            case RIGHT_TRIGGER_DOWN:
                startPosition = StartPosition.B2;
                startingPosition.setValue("B2");
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
                if (allianceColor == AllianceColor.RED) {
                    switch (event.kind) {
                        case RED:
                            particleColor.setValue("RED");
                            detectedRed = true;
                            RobotLog.i("104 Detected red");
                            break;
                        case BLUE:
                            particleColor.setValue("BLUE");
                            detectedRed = false;
                            RobotLog.i("104 Detected blue");
                            break;
                        case BLACK:
                            particleColor.setValue("BLACK");
                            RobotLog.i("104 Detected black");
                            break;
                    }
                } else if (allianceColor == AllianceColor.BLUE) {
                    switch(event.kind) {
                        case RED:
                            particleColor.setValue("RED");
                            detectedRed = true;
                            RobotLog.i("104 Detected red");
                            break;
                        case BLUE:
                            particleColor.setValue("BLUE");
                            detectedRed = false;
                            RobotLog.i("104 Detected blue");
                            break;
                        case BLACK:
                            particleColor.setValue("BLACK");
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
        jewelYServo.setPosition(JEWEL_ARM_DEPLOY);
        addTask(new SingleShotTimerTask(this, 750) {
            @Override
            public void handleEvent(RobotEvent e)
            {
                if (startPosition == startPosition.R1 || startPosition == startPosition.R2) {
                    RobotLog.i("104 Starting initial move.");
                    if((detectedRed == true && allianceColor == AllianceColor.RED || (detectedRed == false && allianceColor == AllianceColor.BLUE))) {
                        jewelXServo.setPosition(JEWEL_ARM_BACK);
                    } else if ((detectedRed == false && allianceColor == AllianceColor.RED) || (detectedRed == true && allianceColor == AllianceColor.BLUE)) {
                        jewelXServo.setPosition(JEWEL_ARM_FORWARD);
                    }
                } else if (startPosition == startPosition.B1 || startPosition == startPosition.B2) {
                    RobotLog.i("104 Starting initial move");
                    if((detectedRed == true && allianceColor == AllianceColor.RED || (detectedRed == false && allianceColor == AllianceColor.BLUE))) {
                        jewelXServo.setPosition(JEWEL_ARM_FORWARD);
                    } else if ((detectedRed == false && allianceColor == AllianceColor.RED) || (detectedRed == true && allianceColor == AllianceColor.BLUE)) {
                        jewelXServo.setPosition(JEWEL_ARM_BACK);
                    }
                }

                addTask(new SingleShotTimerTask(this.robot, 750) {
                    @Override
                    public void handleEvent(RobotEvent e)
                    {
                        jewelYServo.setPosition(JEWEL_ARM_STOW);
                        if (startPosition == startPosition.B1 || startPosition == startPosition.R1) {
                            initialMove(moveToSimplePark);
                        } else if (startPosition == startPosition.B2 || startPosition == startPosition.R2) {
                            initialMove(moveToPark);
                        }
                    }
                });
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
                        if (startPosition == startPosition.R1 || startPosition == startPosition.B1) {
                            RobotLog.i("104 closing glyph arms.");
                            glyphLGrabber.setPosition(GLYPH_CLOSE_LEFT_POSITION);
                            glyphRGrabber.setPosition(GLYPH_CLOSE_RIGHT_POSITION);
                            jewelXServo.setPosition(JEWEL_ARM_NEUTRAL);
                            jewelYServo.setPosition(JEWEL_ARM_STOW);
                        } else if (startPosition == startPosition.R2 || startPosition == startPosition.B2){
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
