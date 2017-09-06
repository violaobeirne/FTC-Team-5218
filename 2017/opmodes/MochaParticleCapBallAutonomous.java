package opmodes;

/*
 * Created by izzielau on 12/17/2016.
 */

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorController;
import com.qualcomm.robotcore.hardware.LightSensor;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.LightSensorCriteria;
import team25core.PersistentTelemetryTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;

@Autonomous(name = "Particle Cap Ball", group = "5218")
public class MochaParticleCapBallAutonomous extends Robot {

    protected enum Alliance {
        BLUE,
        RED,
        DEFAULT,
    }

    protected enum StartingPosition {
        CORNER,
        VORTEX,
        DEFAULT,
    }

    protected enum MoveAfterParticleScore {
        YES,
        NO,
        DEFAULT,
    }

    protected Alliance alliance;
    protected StartingPosition startingPosition;
    protected MoveAfterParticleScore moveAfterParticleScore;

    private static int TURN_MULTIPLY = 0;
    private final int TICKS_PER_INCH = MochaCalibration.TICKS_PER_INCH;
    private final int TICKS_PER_DEGREE = MochaCalibration.TICKS_PER_DEGREE;
    private final double SHOOTER_CORNER = MochaCalibration.SHOOTER_AUTO_CORNER;
    private final double SHOOTER_VORTEX = MochaCalibration.SHOOTER_AUTO_VORTEX;
    private final double MOVE_SPEED = MochaCalibration.MOVE_SPEED;

    private int paddleCount;

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private DcMotor shooterLeft;
    private DcMotor shooterRight;
    private DcMotor sbod;
    private LightSensor rightLight;
    private LightSensor leftLight;
    private FourWheelDirectDrivetrain drivetrain;


    private RunToEncoderValueTask scoreCenterEncoderTask;
    private PersistentTelemetryTask persistentTelemetryTask;
    private GamepadTask gamepad;


    private DeadReckonPath moveAwayFromWallReckonFromCorner;
    private DeadReckonPath moveAwayFromWallReckonFromVortex;

    private DeadReckonPath pushCapDeadReckonFromCorner;
    private DeadReckonPath pushCapDeadReckonFromVortex;

    private LightSensorCriteria whiteLineRightCriteria;
    private LightSensorCriteria whiteLineLeftCriteria;

    @Override
    public void handleEvent(RobotEvent e)
    {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;

            switch (event.kind) {
                case BUTTON_X_DOWN:
                    alliance = Alliance.BLUE;
                    persistentTelemetryTask.addData("ALLIANCE: ", "" + alliance);
                    break;
                case BUTTON_B_DOWN:
                    alliance = Alliance.RED;
                    persistentTelemetryTask.addData("ALLIANCE: ", "" + alliance);
                    break;
                case BUTTON_Y_DOWN:
                    startingPosition = StartingPosition.CORNER;
                    persistentTelemetryTask.addData("POSITION: ", "" + startingPosition);
                    break;
                case BUTTON_A_DOWN:
                    startingPosition = StartingPosition.VORTEX;
                    persistentTelemetryTask.addData("POSITION: ", "" + startingPosition);
                    break;
                case LEFT_BUMPER_DOWN:
                    moveAfterParticleScore = MoveAfterParticleScore.YES;
                    persistentTelemetryTask.addData("MOVE AFTER SHOOTERS: ", "" + moveAfterParticleScore);
                    break;
                case LEFT_TRIGGER_DOWN:
                    moveAfterParticleScore = MoveAfterParticleScore.NO;
                    persistentTelemetryTask.addData("MOVE AFTER SHOOTERS: ", "" + moveAfterParticleScore);
                    break;
            }
        }
        else if (e instanceof RunToEncoderValueTask.RunToEncoderValueEvent) {
            RunToEncoderValueTask.RunToEncoderValueEvent event = (RunToEncoderValueTask.RunToEncoderValueEvent) e;
            handlePaddleEncoderDoneEvent(event);
        }
    }

    @Override
    public void init()
    {
        alliance = Alliance.DEFAULT;
        startingPosition = StartingPosition.DEFAULT;
        moveAfterParticleScore = MoveAfterParticleScore.DEFAULT;

        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        persistentTelemetryTask = new PersistentTelemetryTask(this);
        addTask(persistentTelemetryTask);

        persistentTelemetryTask.addData("ALLIANCE: ", "NOT SELECTED");
        persistentTelemetryTask.addData("POSITION: ", "NOT SELECTED");
        persistentTelemetryTask.addData("MOVE AFTER SHOOTERS: ", "NOT SELECTED");

        frontLeft = hardwareMap.dcMotor.get("motorFL");
        frontRight = hardwareMap.dcMotor.get("motorFR");
        backLeft = hardwareMap.dcMotor.get("motorBL");
        backRight = hardwareMap.dcMotor.get("motorBR");

        shooterLeft = hardwareMap.dcMotor.get("shooterLeft");
        shooterRight = hardwareMap.dcMotor.get("shooterRight");

        sbod = hardwareMap.dcMotor.get("brush");
        paddleCount = 0;

        scoreCenterEncoderTask = new RunToEncoderValueTask(this, sbod, 1, MochaCalibration.BRUSH_SPEED);

        rightLight = hardwareMap.lightSensor.get("lightRight");
        leftLight = hardwareMap.lightSensor.get("lightLeft");
        rightLight.enableLed(false);
        leftLight.enableLed(false);

        frontLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        frontRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        frontRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        backLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        backRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        backRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        drivetrain = new FourWheelDirectDrivetrain(MochaCalibration.TICKS_PER_INCH, frontRight, backRight, frontLeft, backLeft);

        moveAwayFromWallReckonFromCorner = new DeadReckonPath();
        moveAwayFromWallReckonFromCorner.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 5, -0.3);

        moveAwayFromWallReckonFromVortex = new DeadReckonPath();
        moveAwayFromWallReckonFromVortex.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 7, -0.3);

        pushCapDeadReckonFromCorner = new DeadReckonPath();
        pushCapDeadReckonFromCorner.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 64, -MOVE_SPEED);

        pushCapDeadReckonFromVortex = new DeadReckonPath();
        pushCapDeadReckonFromVortex.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 50, -MOVE_SPEED);
        pushCapDeadReckonFromVortex.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, MOVE_SPEED);
        pushCapDeadReckonFromVortex.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -MOVE_SPEED);

    }

    protected void startShooterCorner()
    {
        shooterLeft.setPower(SHOOTER_CORNER);
        shooterRight.setPower(-SHOOTER_CORNER);
    }

    protected void startShooterVortex()
    {
        shooterLeft.setPower(SHOOTER_VORTEX);
        shooterRight.setPower(-SHOOTER_VORTEX);
    }

    protected void stopShooter()
    {
        shooterLeft.setPower(0);
        shooterRight.setPower(0);
    }

    @Override
    public void start()
    {
        if (alliance == Alliance.RED) {
            redInit();
        } else {
            blueInit();
        }
        if (startingPosition == StartingPosition.CORNER) {
            initialMove(moveAwayFromWallReckonFromCorner);
        } else {
            initialMove(moveAwayFromWallReckonFromVortex);
        }
    }

    public void blueInit() {
        TURN_MULTIPLY = 1;
    }

    public void redInit() {
        TURN_MULTIPLY = -1;
    }

    protected void initialMove(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e)
            {
                DeadReckonEvent event = (DeadReckonEvent)e;
                switch(event.kind) {
                    case PATH_DONE:
                        if (startingPosition == StartingPosition.CORNER) {
                            startShooterCorner();
                        } else {
                            startShooterVortex();
                        }

                        addTask(new SingleShotTimerTask(robot, 2000) {
                            @Override
                            public void handleEvent(RobotEvent e)
                            {
                                robot.addTask(scoreCenterEncoderTask);
                            }
                        });
                        break;
                    default:
                        RobotLog.e("163 Unknown event kind");
                        persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Unknown event kind");
                        break;
                }
            }
        });
    }

    protected void handlePaddleEncoderDoneEvent(RunToEncoderValueTask.RunToEncoderValueEvent e)
    {
        switch (e.kind) {
            case DONE:
                if (paddleCount <= 20) {
                    RobotLog.i("163 Paddle count expired, iteration %d", paddleCount);
                    persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Paddle count expired, iteration " + paddleCount);

                    addTask(scoreCenterEncoderTask);
                    paddleCount++;
                } else {
                    RobotLog.i("163 Stopping the shooter");
                    persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Stopping the shooter");

                    stopShooter();

                    if ((moveAfterParticleScore == MoveAfterParticleScore.YES) && (startingPosition == StartingPosition.CORNER)) {
                        RobotLog.i("163 Moving to push cap ball from the corner position");
                        persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Pushing cap ball from corner");

                        handlePaddleCountFinished(pushCapDeadReckonFromCorner);
                    } else if ((moveAfterParticleScore == MoveAfterParticleScore.YES) && (startingPosition == StartingPosition.VORTEX)) {
                        RobotLog.i("163 Moving to push cap ball from the vortex position");
                        persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Pushing cap ball from vortex");

                        handlePaddleCountFinished(pushCapDeadReckonFromVortex);
                    } else {
                        RobotLog.i("163 Robot staying still after shooting a particle");
                        persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Staying still after particle is shot");
                    }
                }
                break;
        }
    }

    protected void handlePaddleCountFinished(DeadReckonPath path)
    {
        RobotLog.i("163 Paddle count finished, moving to cap ball");
        persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Paddle count done, cap ball commence");

        addTask(new DeadReckonTask(this, path, drivetrain) {
            @Override
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                if (event.kind == EventKind.PATH_DONE) {
                    RobotLog.i("163 Robot has finished moving to the cap ball");
                    persistentTelemetryTask.addData("AUTONOMOUS STATE: ", "Autonomous done");
                }
            }
        });
    }
}
