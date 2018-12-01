package opmodes;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;

import java.util.List;

import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.MineralDetectionTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;

import static org.firstinspires.ftc.robotcontroller.internal.FtcRobotControllerActivity.TAG;

/**
 * Created by Lizzie on 10/26/2018.
 */
@Autonomous(name = "League Meet 1 Depot Autonomous")
public class LeagueMeet1HaydnAutonomous extends Robot {
    private enum AllianceColor {
        BLUE,
        RED,
        DEFAULT;
    }

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private Servo bungeeBox;
    private Servo marker;
    private DcMotor lift;
    private FourWheelDirectDrivetrain drivetrain;
    private DeadReckonPath exitDepotPath;

    private GamepadTask gamepad;
    protected AllianceColor allianceColor;
    protected MarkerDropoff.StartingPosition startPosition;
    protected MarkerDropoff.DepotPath depotPath;

    private DeadReckonPath markerPath;
    private MarkerDropoff dropoff;
    private RunToEncoderValueTask landingEncoderTask;

    private Telemetry.Item alliance;
    private Telemetry.Item startingPosition;
    private Telemetry.Item path;
    private Telemetry.Item numberOfMinerals;
    private Telemetry.Item goldMineralPosition;

    @Override
    public void init() {
        allianceColor = allianceColor.DEFAULT;
        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        alliance = telemetry.addData("Alliance: ", "NOT SELECTED");
        startingPosition = telemetry.addData("Starting Position: ", "NOT SELECTED");
        path = telemetry.addData("Depot Path:", "NOT SELECTED");
        numberOfMinerals = telemetry.addData("Number of Minerals: ", "NOT SELECTED");
        goldMineralPosition = telemetry.addData("Gold Mineral Position: ", "NOT SELECTED");

        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");

        bungeeBox = hardwareMap.servo.get("bungeeBox");
        marker = hardwareMap.servo.get("marker");
        lift = hardwareMap.dcMotor.get("lift");
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);

        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();
        dropoff = new MarkerDropoff();
        bungeeBox.setPosition(VivaldiCalibration.BUNGEE_BOX_18);
        marker.setPosition(VivaldiCalibration.MARKER_STOWED);

        exitDepotPath = new DeadReckonPath();
        exitDepotPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, -0.2);

        landingEncoderTask = new RunToEncoderValueTask(this, lift, 8000, VivaldiCalibration.LIFT_DOWN);
    }

    @Override
    public void start() {
        // first landingEncoderTask commands lift to achieve certain encoder value
        markerPath = dropoff.getPath(startPosition, depotPath);
        addTask(landingEncoderTask);
    }

    @Override
    public void handleEvent(RobotEvent e) {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            switch (event.kind) {
                case BUTTON_X_DOWN:
                    allianceColor = AllianceColor.BLUE;
                    alliance.setValue("BLUE");
                    break;
                case BUTTON_B_DOWN:
                    allianceColor = AllianceColor.RED;
                    alliance.setValue("RED");
                    break;
                case BUTTON_A_DOWN:
                    startPosition = MarkerDropoff.StartingPosition.CLOSE;
                    startingPosition.setValue("CLOSE");
                    break;
                case BUTTON_Y_DOWN:
                    startPosition = MarkerDropoff.StartingPosition.FAR;
                    startingPosition.setValue("FAR");
                    break;
                case LEFT_BUMPER_DOWN:
                    depotPath = MarkerDropoff.DepotPath.LEFT;
                    path.setValue("LEFT");
                    break;
                case RIGHT_BUMPER_DOWN:
                    depotPath = MarkerDropoff.DepotPath.RIGHT;
                    path.setValue("RIGHT");
                    break;
            }
        } else if (e instanceof RunToEncoderValueTask.RunToEncoderValueEvent) {
            RunToEncoderValueTask.RunToEncoderValueEvent event = (RunToEncoderValueTask.RunToEncoderValueEvent) e;
            handleLandingDoneEvent(event);
        }
    }

    protected void handleLandingDoneEvent(RunToEncoderValueTask.RunToEncoderValueEvent e)
    {
        // ensures RunToEncoderValueTask is finished before initialMove
        // uses anonymous class declaration because first RunToEncoderValueTask traces to handleEvent
        if (e.kind == RunToEncoderValueTask.EventKind.DONE) {
            addTask(new RunToEncoderValueTask(this, lift, 800, VivaldiCalibration.LIFT_UP) {
                public void handleEvent(RobotEvent e) {
                    RunToEncoderValueTask.RunToEncoderValueEvent event = (RunToEncoderValueTask.RunToEncoderValueEvent)e;
                    if (event.kind == EventKind.DONE) {
                        initialMove(markerPath);
                    }
                }
            });
        }
    }

    protected void initialMove(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        dropMarker();
                }
            }
        });
    }

    protected void dropMarker() {
        addTask(new SingleShotTimerTask(this, 600) {
            @Override
            public void handleEvent(RobotEvent e) {
                marker.setPosition(VivaldiCalibration.MARKER_DEPLOYED);
                exitDepot(exitDepotPath);
            }
        });
    }

    protected void exitDepot(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                    // insert crater parking path later on
                }
            }
        });
    }
}
