package opmodes;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Gamepad;
import com.qualcomm.robotcore.hardware.Servo;

import org.firstinspires.ftc.robotcore.internal.android.dex.Leb128;

import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.PersistentTelemetryTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 11/11/2017.
 */

@Autonomous (name = "5218 Glyph Autonomous")
public class LisztParkAutonomous extends Robot{
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

    protected boolean isRedAlliance;

    private static int TURN_MULTIPLIER = 0;
    private static int MOVE_MULTIPLIER = 0;

    private final double TURN_SPEED = HisaishiCalibration.TURN_SPEED;
    private final double MOVE_SPEED = HisaishiCalibration.MOVE_SPEED;

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private Servo glyphLGrabber;
    private Servo glyphRGrabber;

    private PersistentTelemetryTask persistentTelemetryTask;
    private GamepadTask gamepad;

    private DeadReckonPath moveToSimplePark;
    private DeadReckonPath moveToPark;

    private FourWheelDirectDrivetrain drivetrain;

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
        isRedAlliance = false;

        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        persistentTelemetryTask = new PersistentTelemetryTask(this);
        addTask(persistentTelemetryTask);

        persistentTelemetryTask.addData("ALLIANCE: ", "NOT SELECTED");
        persistentTelemetryTask.addData("STARTING POSITION: ", "NOT SELECTED");

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

        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);

        moveToSimplePark = new DeadReckonPath();
        moveToPark = new DeadReckonPath();

        glyphLGrabber.setPosition(0.8);
        glyphRGrabber.setPosition(0.2);
    }

    @Override
    public void start () {
        if (alliance == Alliance.RED) {
            isRedAlliance = true;
            if (startingPosition == StartingPosition.R1) {
                red1Init();
                moveToSimplePark.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 42, MOVE_MULTIPLIER * MOVE_SPEED);
                glyphLGrabber.setPosition(0);
                glyphRGrabber.setPosition(1.0);
                addTask(new SingleShotTimerTask(this, 1000) {
                    @Override
                    public void handleEvent(RobotEvent e)
                    {
                        initialMove(moveToSimplePark);
                    }
                });
            } else if (startingPosition == StartingPosition.R2) {
                red2Init();
                glyphLGrabber.setPosition(0);
                glyphRGrabber.setPosition(1.0);
                moveToPark.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 42, MOVE_MULTIPLIER * MOVE_SPEED);
                addTask(new SingleShotTimerTask(this, 1000) {
                    @Override
                    public void handleEvent(RobotEvent e)
                    {
                        initialMove(moveToPark);
                    }
                });
            }
        } else if (alliance == Alliance.BLUE) {
            isRedAlliance = false;
            if (startingPosition == StartingPosition.B1) {
                blue1Init();
                glyphLGrabber.setPosition(0);
                glyphRGrabber.setPosition(1.0);
                moveToSimplePark.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 42, MOVE_MULTIPLIER * MOVE_SPEED);
                addTask(new SingleShotTimerTask(this, 1000) {
                    @Override
                    public void handleEvent(RobotEvent e)
                    {
                        initialMove(moveToSimplePark);

                    }
                });
            } else if (startingPosition == StartingPosition.B2) {
                blue2Init();
                glyphLGrabber.setPosition(0);
                glyphRGrabber.setPosition(1.0);
                moveToPark.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 42, MOVE_MULTIPLIER * MOVE_SPEED);
                addTask(new SingleShotTimerTask(this, 1000) {
                    @Override
                    public void handleEvent(RobotEvent e)
                    {
                        initialMove(moveToPark);
                    }
                });
            }
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
        MOVE_MULTIPLIER = -1;
        TURN_MULTIPLIER = -1;
    }

    protected void blue2Init() {
        MOVE_MULTIPLIER = -1;
        TURN_MULTIPLIER = -1;
    }

    protected void initialMove(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        if (startingPosition == StartingPosition.R1 || startingPosition == StartingPosition.B1) {
                            glyphLGrabber.setPosition(1.0);
                            glyphRGrabber.setPosition(0);
                        } else if (startingPosition == StartingPosition.R2 || startingPosition == StartingPosition.B2){
                            glyphLGrabber.setPosition(1.0);
                            glyphRGrabber.setPosition(0);
                        }
                }
            }
        });
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
}
