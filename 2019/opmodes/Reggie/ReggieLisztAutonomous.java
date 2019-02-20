package opmodes.Reggie;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;

import java.util.List;

import opmodes.Utilities.VivaldiCalibration;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.MineralDetectionTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 11/27/2018.
 */
@Autonomous(name = "Reggie Master Autonomous")
public class ReggieLisztAutonomous extends Robot {
    /*
    public enum Hanging {
        HANGING,
        NOT_HANGING,
        DEFAULT,
    }
    */

    // declaring motors, servos, and drivetrain
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private FourWheelDirectDrivetrain drivetrain;
    private DcMotor liftLeft;
    private DcMotor liftRight;

    private DeadReckonPath knockPath;
    private ReggieDropoffUtil dropoff;

    private DeadReckonPath exitDepotPath;
    private ReggieExitDepotDropoff exitDepotDropoff;

    private boolean hangingBoolean = false;
    private DcMotor bungeeBox;

    // declaring gamepad variables
    private GamepadTask gamepad1;
    private GamepadTask gamepad2;
    protected ReggieDropoffUtil.MineralPosition goldMineralPosition;
    protected ReggieDropoffUtil.StartingPosition robotStartingPosition;
    protected ReggieDropoffUtil.EndingPosition robotEndingPosition;


    // declaring telemetry item
    private Telemetry.Item numberOfMineralsItem;
    private Telemetry.Item goldMineralPositionItem;
    private Telemetry.Item startingPositionItem;
    private Telemetry.Item hangingItem;
    private Telemetry.Item endingPositionItem;

    @Override
    public void init() {
        // drivetrain initialization
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();

        // lift initialization
        liftLeft = hardwareMap.dcMotor.get("liftLeft");
        liftRight = hardwareMap.dcMotor.get("liftRight");
        liftLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        liftRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);

        // bungee box initialization
        bungeeBox = hardwareMap.dcMotor.get("bungeeBox");
        bungeeBox.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        bungeeBox.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        // bungeeBox.setZeroPowerBehavior(DcMotor.ZeroPowerBehavior.BRAKE);

        // depot path
        knockPath = new DeadReckonPath();
        dropoff = new ReggieDropoffUtil();
        exitDepotPath = new DeadReckonPath();
        exitDepotDropoff = new ReggieExitDepotDropoff();

        // initializing gamepad variables
        gamepad1 = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad1);
        gamepad2 = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2);
        addTask(gamepad2);

        // initializing telemetry items
        numberOfMineralsItem = telemetry.addData("Number of Minerals: ", "NOT DETECTED");
        goldMineralPositionItem = telemetry.addData("Gold Mineral Position: ", "NOT SELECTED");
        startingPositionItem = telemetry.addData("Starting Position", "NOT SELECTED");
        hangingItem = telemetry.addData("Hanging: ", "NOT SELECTED");
        endingPositionItem = telemetry.addData("Ending Position: ", "NOT SELECTED");

        // initializing mineral detection and marker movement
        initializeMineralDetection();
    }

    protected void initializeMineralDetection() {
        String cameraName = "mineralCamera";
        MineralDetectionTask mdTask = new MineralDetectionTask(this, cameraName) {
            public void handleEvent(RobotEvent e) {
                MineralDetectionEvent event = (MineralDetectionEvent) e;
                List<Recognition> updatedMinerals = event.minerals;
                numberOfMineralsItem.setValue(updatedMinerals.size());
                ReggieDropoffUtil.MineralPosition goldPos = ReggieDropoffUtil.determineGoldPosition(updatedMinerals);
                ReggieDropoffUtil.sendPositionTelemetry(goldPos, goldMineralPositionItem);

                switch (goldPos) {
                    case LEFT:
                        goldMineralPosition = ReggieDropoffUtil.MineralPosition.LEFT;
                        break;
                    case RIGHT:
                        goldMineralPosition = ReggieDropoffUtil.MineralPosition.RIGHT;
                        break;
                    case CENTER:
                        goldMineralPosition = ReggieDropoffUtil.MineralPosition.CENTER;
                        break;
                    case UNKNOWN:
                        goldMineralPosition = ReggieDropoffUtil.MineralPosition.CENTER;
                        break;
                }
            }
        };
        mdTask.init(telemetry, hardwareMap);
        mdTask.setDetectionKind(MineralDetectionTask.DetectionKind.EVERYTHING);
        this.addTask(mdTask);
    }

    @Override
    public void start() {
        if(hangingBoolean == true) {
            unlatch();
        } else if (hangingBoolean == false) {
            knockPath = dropoff.getPath(robotStartingPosition, robotEndingPosition, goldMineralPosition);
            exitDepotPath = exitDepotDropoff.getPath(goldMineralPosition);
            initialMove(knockPath);
        }
    }

    @Override
    public void handleEvent(RobotEvent e) {
        if (e instanceof GamepadTask.GamepadEvent) {
            // starting position, hanging, parking, gold mineral position, double sampling
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            switch (event.kind) {
                case BUTTON_X_DOWN:
                    endingPositionItem.setValue("PARKING");
                    robotEndingPosition = ReggieDropoffUtil.EndingPosition.PARKING;
                    break;
                case BUTTON_Y_DOWN:
                    endingPositionItem.setValue("NOT PARKING");
                    robotEndingPosition = ReggieDropoffUtil.EndingPosition.NOT_PARKING;
                    break;
                case BUTTON_A_DOWN:
                    endingPositionItem.setValue("DOUBLE SAMPLING");
                    robotEndingPosition = ReggieDropoffUtil.EndingPosition.DOUBLE_SAMPLING;
                    break;
                case RIGHT_BUMPER_DOWN:
                    startingPositionItem.setValue("DEPOT");
                    robotStartingPosition = ReggieDropoffUtil.StartingPosition.DEPOT;
                    break;
                case RIGHT_TRIGGER_DOWN:
                    startingPositionItem.setValue("CRATER");
                    robotStartingPosition = ReggieDropoffUtil.StartingPosition.CRATER;
                    break;
                case LEFT_BUMPER_DOWN:
                    hangingItem.setValue("HANGING");
                    hangingBoolean = true;
                    break;
                case LEFT_TRIGGER_DOWN:
                    hangingItem.setValue("NOT HANGING");
                    hangingBoolean = false;
                    break;
            }
        }
    }

    // add the prompt to initial move after unlatch
    public void unlatch() {
        RunToEncoderValueTask leftTask = new RunToEncoderValueTask(this, liftLeft, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_LEFT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    liftLeft.setPower(0.0);
                }
            }
        };
        RunToEncoderValueTask rightTask = new RunToEncoderValueTask(this, liftRight, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_RIGHT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    liftRight.setPower(0.0);
                }
            }
        };
        addTask(leftTask);
        addTask(rightTask);
    }

    protected void initialMove(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        if (robotStartingPosition == ReggieDropoffUtil.StartingPosition.DEPOT) {
                            markerDrop();
                        } else {
                            exitDepot(exitDepotPath);
                        }
                        break;
                }
            }
        });
    }

    protected void markerDrop() {
        RobotLog.i("Carrying out marker drop method.");
        addTask(new SingleShotTimerTask(this, 600) {
            @Override
            public void handleEvent(RobotEvent e) {
                exitDepot(exitDepotPath);
            }
        });
    }

    protected void exitDepot(final DeadReckonPath path) {
        RobotLog.i("Carrying out exit depot method.");
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        RobotLog.i("251: You're done, congrats!");
                }
            }
        });
    }

}
